{-# LANGUAGE TypeOperators #-}


{-|
Module      : Compiler
Description : Cheesecloth compiler
Maintainer  : santiago@galois.com
Stability   : prototype

The cheesecloth compiler translates LLVM modules to MicroRAM as diagramed below:
  @
   +---------+
   |  LLVM   |
   +----+----+
        | Instruction selection
   +----v----+
   |         +---+
c  |         |   | Rename Intrinsics / Lower intrinsics
o  |         <---+
m  | MicroIR |
p  |         +---+
i  |         |   | Check for Undefined Functions
l  |         <---+
e  |         |
1  +----+----+
        | Legalize
   +----v----+
   |         +---+
   |         |   | Localize labels
   |         <---+
   |         |
   |         +---+
   |         |   | Edge Split
   |         <---+
   |         |
   |   RTL   +---+                 
   |         |   | Lower Extension Instrs (depends on run: hi/low)
   |         <---+
   |         |
   |         +---+
   |         |   | Remove Phi
   |         <---+
   |         |
   +----+----+
        | Register allocation
   +----v----+
   |         +---+
   |   LTL   |   | Calling conventions
   |         <---+
   +----+----+
        | Stacking
   +----v----+     Sparsity
   |MicroASM +------------->
   |         +---+
   |         |   | Block Cleanup
   |         <---+
   +----+----+
        | Label removal
   +----v----+
   |MicroRAM |
   +---------+
   @


= Source, intermediate and target languages: syntax and semantics

* __LLVM__ The LLVM assembly language as defined in <https://llvm.org/docs/LangRef.html>.
  We use the haskell implementation of `llvm-hs`.

* __MicroIR__ High-level IR based on MicroRAM.  Includes MicroRAM instructions with
   support for extended operand kinds and two non-register operands per
   instruction (normal MicroRAM requires one operand to be a register), as well
   as extended high-level instructions (see `MIRprog`).

* __RTL__ : Register Transfer language (RTL) uses infinite virtual registers,
  function calls, Stack allocation and regular MicroRAM instructions (see `Rprog`). 

* __LTL__ Location Transfer Language (LTL) is close to RTL but uses machine registers
  and stack slots instead of virtual registers (see `Lprog`). 

* __MicroASM__ : MicroRAM assembly langues. It enhances MicroRAM with support for
  global variables and code labels

* __MicroRAM__ : A simple machine language designed for zero knowledge
execution of programs.

= Compiler passes

* __Instruction Selection__ (`instrSelect`): Instruction selection translates LLVM to
  MicroIR. It's a linear pass that translates each LLVM instruction to 0 or MicroIR
  instructinos. For now, it does not combine instructinos.

* __Intrinsics__ (`renameLLVMIntrinsicImpls`, lowerIntrinsics): Inlines calls to
  intrinsics to MicroASM instructions. Renames the intrinsics from using underscors ("__") to
  using dot (".") and removes the empty bodies of those functions.

* _Check undefined functions_ (`checkUndefinedFuncs`): Produces an error if, at this point,
  any function has an empty body.

* __Legalize__ (`legalize`): This module compiles MicroIR to RTL.  It looks for
  instruction formats that aren't legal in RTL/MicroRAM, such as addition of two
  constants, and replaces them with legal sequences.  Usually this means adding an
  `Imov` to put one operand into a register, but for some instructions we can do better.

* __Localize labels__ (`localizeLabels`): Rename blocks local to a function to avoid conflicts
between functions.

* __Edge Split__ (`edgeSplit`) :  Split edges between blocks to ensure the unique
  successor or predecessor property.

* __Remove Phi__ (`removePhi`) : Removes phi functions inherited from the SSA form.

* __Register Allocation__ (`registerAlloc`): Register allocation

* __Calling Convention__ (`callingConvention`): Saving callee saved registersto the stack.

* __Stacking__ (`stacking`): Lays out the stack in memory. Moreover this pass adds
  the necessary instructions from stack frame creation and destruction on function
  call and return.

* __Sparsity analysis__ (`sparsity`): Estimates the minimum distance between two
  executions of the same opcode or op codes of the same class. For example, it can
  estimate the distance between memory operations. This pass is pureley analitical
  and does not modify the program. 

* __Remove Labels__ (`removeLabels`): Translates MicroASM to MicroRAM by
  removing code labels and replacing them with constant code pointers.

-}
module Compiler
    ( compile
    , CompilerOptions(..), defOptions
    , Domain(..), DomainInput(..), loadCode, inputPath, compileDomains
    , module Export
    ) where

import           Compiler.Analysis
import           Compiler.BlockCleanup
import           Compiler.CallingConvention
import           Compiler.Common (GlobalVariable)
import           Compiler.CompilationUnit
import           Compiler.CountFunctions
import           Compiler.Errors
import           Compiler.Extension
import           Compiler.IRs
import           Compiler.InstructionSelection
import           Compiler.Intrinsics
import           Compiler.LayArgs
import           Compiler.Legalize
import           Compiler.Link
import           Compiler.LocalizeLabels
import           Compiler.Metadata
import           Compiler.Name (firstUnusedName)
import           Compiler.RegisterAlloc
import           Compiler.RegisterAlloc as Export (AReg)
import           Compiler.RemoveLabels
import           Compiler.RemovePhi
import           Compiler.Stacking
import           Compiler.UndefinedFunctions

import           MicroRAM (MWord)
import           RiscV.Intrinsics
import           RiscV.Parser
import           RiscV.Transpiler
import           Sparsity.Sparsity
import           Util.Util

import           Data.Default
import           Debug.Trace

import qualified LLVM.AST as LLVM

-- When verbose, shows the current compilation pass for debugging. 
verbTagPass :: Bool -> String -> (a -> Hopefully b) -> a -> Hopefully b
verbTagPass verb txt x =
  (if verb then trace ("\tCompiler Pass: " <> txt) else id) $ tagPass txt x

data CompilerOptions = CompilerOptions
  { verb::Bool
  , allowUndefFun::Bool
  , spars::Maybe Int
  , tainted::Bool
  , skipRegisterAllocation::Bool
  , numberRegs::Maybe Word
  , riscvEmulatorEnabled::Bool
  }

defOptions :: CompilerOptions
defOptions = CompilerOptions
  { verb = False
  , allowUndefFun = False
  , spars = Nothing
  , tainted = False
  , skipRegisterAllocation = False
  , numberRegs = Nothing
  , riscvEmulatorEnabled = False
  }

compile1
  :: CompilerOptions
  -> Word
  -> LLVM.Module
  -> Word
  -> Hopefully (CompilationUnit () (MIRprog Metadata MWord))
compile1 options len llvmProg firstName = (return $ prog2unit len llvmProg firstName)
  >>= (verbTagPass verb "Instruction Selection" $ justCompileWithNames instrSelect)
  >>= (verbTagPass verb "Rename LLVM Intrinsic Implementations" $ justCompile renameLLVMIntrinsicImpls)
  >>= (verbTagPass verb "Lower Intrinsics" $ justCompileWithNamesSt lowerIntrinsics)
  where CompilerOptions {verb=verb, allowUndefFun=_allowUndefFun} = options

compileCheckUndef
  :: CompilerOptions
  -> CompilationUnit () (MIRprog Metadata MWord)
  -> Hopefully (CompilationUnit () (MIRprog Metadata MWord))
compileCheckUndef options prog = return prog
  >>= (verbTagPass verb "Catch undefined Functions" $ justCompile (catchUndefinedFunctions allowUndefFunOption))
  where CompilerOptions {verb=verb, allowUndefFun=allowUndefFunOption} = options

compileLowerExt
  :: CompilerOptions
  -> CompilationUnit g (MIRprog Metadata MWord)
  -> Hopefully (CompilationUnit g (MIRprog Metadata MWord))
compileLowerExt options = verbTagPass (verb options) "Lower Extension Instructions" $
  justCompileWithNames lowerExtensionInstrs

compile2
  :: CompilerOptions
  -> CompilationUnit () (MIRprog Metadata MWord)
  -> Hopefully (CompilationUnit [GlobalVariable MWord] (MAProgram Metadata AReg MWord))
compile2 options prog = return prog
  >>= (verbTagPass verb "Legalize Instructions" $ justCompileWithNames legalize)
  >>= (verbTagPass verb "Localize Labels"     $ justCompileWithNames localizeLabels)
  >>= (verbTagPass verb "Edge split"          $ justCompileWithNames edgeSplit)
  >>= (verbTagPass verb "Remove Phi Nodes"    $ justCompileWithNames removePhi)
  >>= (verbTagPass verb "Layout arguments"    $ justCompileWithNamesSt layArgs)
  >>= (verbTagPass verb "Register Allocation" $ registerAlloc skipRegisterAllocation numRegs)
  >>= (verbTagPass verb "Calling Convention"  $ justCompile callingConvention)
  >>= (verbTagPass verb "Stash Globals"       $ return . stashGlobals)
  >>= (verbTagPass verb "Count Functions"     $ justAnalyse countFunctions)
  >>= (verbTagPass verb "Stacking"            $ justCompileWithNames stacking)
  >>= (verbTagPass verb "Computing Sparsity"  $ justAnalyse (return . SparsityData . (forceSparsity spars))) 
  where numRegs = case numberRegs of
          Just n -> RegisterAllocOptions n
          Nothing -> def

        CompilerOptions { verb=verb
                        , spars=spars
                        , skipRegisterAllocation=skipRegisterAllocation
                        , numberRegs=numberRegs
                        } = options

compileRiscvPremain
  :: CompilerOptions
  -> CompilationUnit [GlobalVariable MWord] (MAProgram Metadata AReg MWord)
  -> Hopefully (CompilationUnit [GlobalVariable MWord] (MAProgram Metadata AReg MWord))
compileRiscvPremain options prog = return prog
  >>= (verbTagPass verb "Add premain"         $ return . addRiscvPremain)
  where CompilerOptions { verb=verb } = options

compileLLVMPremain
  :: CompilerOptions
  -> CompilationUnit [GlobalVariable MWord] (MAProgram Metadata AReg MWord)
  -> Hopefully (CompilationUnit [GlobalVariable MWord] (MAProgram Metadata AReg MWord))
compileLLVMPremain options prog = return prog
  >>= (verbTagPass verb "Add premain"         $ justCompileWithNames addPremain)
  where CompilerOptions { verb=verb } = options

compileBlockCleanup
  :: CompilerOptions
  -> CompilationUnit [GlobalVariable MWord] (MAProgram Metadata AReg MWord)
  -> Hopefully (CompilationUnit [GlobalVariable MWord] (MAProgram Metadata AReg MWord))
compileBlockCleanup options prog = return prog
  >>= (verbTagPass verb "Block cleanup"       $ blockCleanup)
  where CompilerOptions { verb=verb } = options

compile3
  :: CompilerOptions
  -> CompilationUnit [GlobalVariable MWord] (MAProgram Metadata AReg MWord)
  -> Hopefully (CompilationUnit () (AnnotatedProgram Metadata AReg MWord))
compile3 options prog = return prog
  >>= (verbTagPass verb "Removing labels"     $ removeLabels tainted)
  where CompilerOptions { verb=verb, tainted=tainted } = options

compile :: CompilerOptions
        -> Word
        -> LLVM.Module
        -> Hopefully $ CompilationResult (AnnotatedProgram Metadata AReg MWord)
compile options len llvmProg = do
  ir <- compile1 options len llvmProg firstUnusedName
    >>= compileCheckUndef options
  high <- return ir
    >>= compile2 options
    >>= compileLLVMPremain options
    >>= compileBlockCleanup options
    >>= compile3 options
  low <- return ir
    >>= compileLowerExt options
    >>= compile2 options
    >>= compileLLVMPremain options
    >>= compileBlockCleanup options
    >>= compile3 options
  -- Return both programs, using the analysis data from the final one.
  return $ low { programCU = MultiProg (programCU high) (programCU low) }


data Domain = Domain
  { secretLengths :: Maybe (Word, Word)
  -- ^ If set, the code and memory of the domain are secret, with the indicated
  -- upper limits on their respective lengths.
  , privileged :: Bool
  -- ^ If set, all code and memory of the domain will be placed in the upper
  -- (privileged) portion of memory.
  , domainInputs :: [DomainInput]
  -- ^ List of inputs to compile to construct this domain.  This may be empty;
  -- in particular, in verifier mode there will usually be no code available
  -- for secret domains.
  }
  deriving (Eq, Show)

-- | An input file for a domain.  In each constructor, the `Maybe String` is
-- the contents of the file, which initially is unset.
data DomainInput =
    InputLLVM FilePath (Maybe LLVM.Module)
  | InputRISCV FilePath (Maybe String)
  deriving (Eq, Show)

inputPath :: DomainInput -> FilePath
inputPath (InputLLVM path _) = path
inputPath (InputRISCV path _) = path


data CompiledObject = CompiledObject
  { objLow :: CompilationUnit [GlobalVariable MWord] (MAProgram Metadata AReg MWord)
  , objHigh :: CompilationUnit [GlobalVariable MWord] (MAProgram Metadata AReg MWord)
  , objNextName :: Word
  }


loadCode :: Applicative m
         => (FilePath -> m LLVM.Module)
         -> (FilePath -> m String)
         -> Domain
         -> m Domain
loadCode loadLLVM loadRISCV domain =
  (\inputs' -> domain { domainInputs = inputs' }) <$> traverse go (domainInputs domain)
  where
    go (InputLLVM path _) = InputLLVM path <$> (Just <$> loadLLVM path)
    go (InputRISCV path _) = InputRISCV path <$> (Just <$> loadRISCV path)


compileDomains :: CompilerOptions
               -> Word
               -> [Domain]
               -> Hopefully $ CompilationResult (AnnotatedProgram Metadata AReg MWord)
compileDomains options len domains = do
  (objs, _nextName) <- sequenceObjects firstUnusedName
    [compileDomain options len d | d <- domains]
  obj <- concatObjects objs
  -- Hack: add the appropriate premain corresponding to the first input of the
  -- first domain.
  let compilePremain = case concatMap domainInputs domains of
        InputLLVM _ _ : _ -> compileLLVMPremain
        InputRISCV _ _ : _ -> compileRiscvPremain
        [] -> error "empty domain"
  high <- return (objHigh obj)
    >>= compilePremain options
    >>= compile3 options
  low <- return (objLow obj)
    >>= compilePremain options
    >>= compile3 options
  return $ low { programCU = MultiProg (programCU high) (programCU low) }

riscvIntrinsicsObj :: Word -> Hopefully CompiledObject
riscvIntrinsicsObj nextName = do
  cu <- riscvIntrinsicsCU nextName
  return $ CompiledObject cu cu (nameBound cu)

compileDomain :: CompilerOptions
              -> Word
              -> Domain
              -> Word
              -> Hopefully CompiledObject
compileDomain options len domain _firstName = do
  let inputObjsM = [compileInput options len inp | inp <- domainInputs domain]
  -- Hack: add intrinsics or not depending on the first input of the domain.
  let extraObjsM = case domainInputs domain of
        InputLLVM _ _ : _ -> []
        InputRISCV _ _ : _ -> [riscvIntrinsicsObj]
        [] -> error "empty domain"
  (objs, _nextName) <- sequenceObjects firstUnusedName (inputObjsM ++ extraObjsM)
  obj <- linkObjects objs
  high <- return (objHigh obj)
  low <- return (objLow obj)
    >>= (verbTagPass verb "Lower Extension Instructions" $ justCompile lowerExtensionInstrsAsm)
  let nextName = max (nameBound high) (nameBound low)
  return $ CompiledObject low high nextName
  where CompilerOptions { verb=verb } = options

compileInput :: CompilerOptions
             -> Word
             -> DomainInput
             -> Word
             -> Hopefully CompiledObject
compileInput options len (InputLLVM path mCode) firstName = do
  let code = maybe (error $ "missing code for " ++ show path) id mCode
  ir <- compile1 options len code firstName
  high <- return ir
    >>= compile2 options
  low <- return ir
    >>= compileLowerExt options
    >>= compile2 options
  let nextName = max (nameBound high) (nameBound low)
  return $ CompiledObject low high nextName
compileInput options len (InputRISCV path mCode) firstName = do
  let code = maybe (error $ "missing code for " ++ show path) id mCode
  parsed <- riscvParser path code
  masm <- transpiler verb riscvEmulatorEnabled firstName mempty parsed
  let masm' = masm { traceLen = len }
  return $ CompiledObject masm' masm' (nameBound masm')
  where CompilerOptions { verb=verb, riscvEmulatorEnabled=riscvEmulatorEnabled } = options

sequenceObjects :: Word -> [Word -> Hopefully CompiledObject] -> Hopefully ([CompiledObject], Word)
sequenceObjects firstName mkObjs = go firstName mkObjs
  where go firstName [] = return ([], firstName)
        go firstName (f : fs) = do
          obj <- f firstName
          (objs, nextName) <- go (objNextName obj) fs
          return (obj:objs, nextName)

-- | Link together several objects.  This adjusts certain `Name`s so that
-- objects can refer to external symbols (blocks and globals) defined in other
-- objects.
linkObjects :: [CompiledObject] -> Hopefully CompiledObject
linkObjects objs = do
  high <- linkCompUnits (map objHigh objs)
  low <- linkCompUnits (map objLow objs)
  return $ CompiledObject
    { objHigh = high
    , objLow = low
    , objNextName = max (nameBound high) (nameBound low)
    }

-- | Concatenate several objects.  This makes no changes to `Name`s, so symbols
-- defined in one object won't be visible in the others.
concatObjects :: [CompiledObject] -> Hopefully CompiledObject
concatObjects objs = do
  high <- concatCompUnits (map objHigh objs)
  low <- concatCompUnits (map objLow objs)
  return $ CompiledObject
    { objHigh = high
    , objLow = low
    , objNextName = max (nameBound high) (nameBound low)
    }
