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
   |         |   | Calling conventions
   |         <---+
   |   LTL   |
   |         +---+
   |         |   | Globals
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

* __Gloabls__ (`replaceGlobals`): Converts globals into initial memory and replaces all
  uses of global variables with the constant pointer to their
  location in memory.         
                              
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
    , module Export
    ) where

import           Data.Default

import Compiler.Analysis
import Compiler.BlockCleanup
import Compiler.CallingConvention
import Compiler.CompilationUnit
import Compiler.CountFunctions
import Compiler.Globals
import Compiler.Errors
import Compiler.Extension
import Compiler.InstructionSelection
import Compiler.Intrinsics
import Compiler.IRs
import Compiler.LocalizeLabels
import Compiler.Legalize
import Compiler.Metadata
import Compiler.RegisterAlloc
import Compiler.RegisterAlloc as Export (AReg)
import Compiler.RemoveLabels
import Compiler.RemovePhi
import Compiler.Stacking
import Compiler.UndefinedFunctions

import MicroRAM (MWord)

import Sparsity.Sparsity

import qualified LLVM.AST as LLVM
import Util.Util

compile1
  :: Bool
  -> Word
  -> LLVM.Module
  -> Hopefully (CompilationUnit () (Rprog Metadata MWord))
compile1 allowUndefFun len llvmProg = (return $ prog2unit len llvmProg)
  >>= (tagPass "Instruction Selection" $ justCompile instrSelect)
  >>= (tagPass "Rename LLVM Intrinsic Implementations" $ justCompile renameLLVMIntrinsicImpls)
  >>= (tagPass "Lower Intrinsics" $ justCompile lowerIntrinsics)
  >>= (tagPass "Catch undefined Functions" $ justCompile (catchUndefinedFunctions allowUndefFun))
  >>= (tagPass "Legalize Instructions" $ justCompile legalize)
  >>= (tagPass "Localize Labels" $ justCompile localizeLabels)
  >>= (tagPass "Edge split" $ justCompile edgeSplit)

compile2
  :: Maybe Int
  -> CompilationUnit () (Rprog Metadata MWord) ->
  Hopefully (CompilationUnit () (AnnotatedProgram Metadata AReg MWord))
compile2 spars prog = return prog
  >>= (tagPass "Edge split"          $ justCompile edgeSplit)
  >>= (tagPass "Remove Phi Nodes"    $ justCompile removePhi)
  >>= (tagPass "Register Allocation" $ registerAlloc def)
  >>= (tagPass "Calling Convention"  $ justCompile callingConvention)
  >>= (tagPass "Remove Globals"      $ replaceGlobals)
  >>= (tagPass "Count Funcitons"     $ justAnalyse countFunctionsEmpty)
  >>= (tagPass "Stacking"            $ justCompile stacking)
  >>= (tagPass "Computing Sparsity"  $ justAnalyse (return . SparsityData . (forceSparsity spars))) 
  >>= (tagPass "Block cleanup"       $ blockCleanup)
  >>= (tagPass "Removing labels"     $ removeLabels)

compile :: Bool -> Word -> LLVM.Module -> Maybe Int -> Hopefully $ CompilationResult (AnnotatedProgram Metadata AReg MWord)
compile allowUndefFun len llvmProg spars = do
  ir <- compile1 allowUndefFun len llvmProg
  high <- compile2 spars ir
  low <- return ir
    >>= (tagPass "Lower Extension Instructions" $ justCompile lowerExtensionInstrs)
    >>= compile2 spars
  -- Return both programs, using the analysis data from the final one.
  return $ low { programCU = MultiProg (programCU high) (programCU low) }
