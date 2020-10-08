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
  | MicroIR |
  +----+----+
       | Legalize
  +----v----+
  |   RTL   |
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

* __Legalize__ (`legalize`): This module compiles MicroIR to RTL.  It looks for
  instruction formats that aren't legal in RTL/MicroRAM, such as addition of two
  constants, and replaces them with legal sequences.  Usually this means adding an
  `Imov` to put one operand into a register, but for some instructions we can do better.

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

* __Remove Labels__ (`removeLabels`): Compiles Translates MicroASM to MicroRAM by
  removing code labels and replacing them with constant code pointers.

-}
module Compiler
    ( compile --compileStraight
    , module Export
    ) where

import qualified LLVM.AST as LLVM
import           Data.Default

import Util.Util

import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.IRs
import Compiler.InstructionSelection
import Compiler.Intrinsics
import Compiler.Legalize
import Compiler.Extension
import Compiler.RemovePhi
import Compiler.RegisterAlloc
import Compiler.RegisterAlloc as Export (AReg)
import Compiler.CallingConvention
import Compiler.Globals
import Compiler.Stacking
import Compiler.Sparsity
import Compiler.RemoveLabels
import Compiler.Analysis
import Compiler.LocalizeLabels

import MicroRAM (MWord)
import qualified MicroRAM as MRAM  (Program) 

(<.>) :: Monad m => (b -> c) -> (a -> b) -> a -> m c
f <.> g = \x -> return $ f $ g x 

compile1 :: Word -> LLVM.Module ->
  Hopefully (CompilationUnit () (Rprog () MWord))
compile1 len llvmProg = (return $ prog2unit len llvmProg)
  >>= (tagPass "Instruction Selection" $ justCompile instrSelect)
  >>= (tagPass "Rename LLVM Intrinsic Implementations" $ justCompile renameLLVMIntrinsicImpls)
  >>= (tagPass "Lower Intrinsics" $ justCompile lowerIntrinsics)
  >>= (tagPass "Legalize Instructions" $ justCompile legalize)
  >>= (tagPass "Localize Labels" $ justCompile localizeLabels)
  >>= (tagPass "Edge split" $ justCompile edgeSplit)

compile2 :: CompilationUnit () (Rprog () MWord) ->
  Hopefully (CompilationUnit () (MRAM.Program AReg MWord))
compile2 prog = return prog
  >>= (tagPass "Remove Phi Nodes" $ justCompile removePhi)
  >>= (tagPass "Register Allocation" $ registerAlloc def)
  >>= (tagPass "Calling Convention" $ justCompile callingConvention)
  >>= (tagPass "Remove Globals" $ replaceGlobals)
  >>= (tagPass "Stacking" $ justCompile stacking)
  >>= (tagPass "Computing Sparsity" $ justAnalyse (SparsityData <.> sparsity)) 
  >>= (tagPass "Removing labels" $ removeLabels)

compile :: Word -> LLVM.Module
        -> Hopefully $ CompilationResult (MRAM.Program AReg MWord)
compile len llvmProg = do
  ir <- compile1 len llvmProg
  high <- compile2 ir
  low <- return ir
    >>= (tagPass "Lower Extension Instructions" $ justCompile lowerExtensionInstrs)
    >>= compile2
  -- Return both programs, using the analysis data from the final one.
  return $ low { programCU = MultiProg (programCU high) (programCU low) }
