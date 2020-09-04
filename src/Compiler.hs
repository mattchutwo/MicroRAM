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
  |   LTL   |   |Globals
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
    ( compile, --compileStraight
    ) where

import qualified LLVM.AST as LLVM
import           Data.Default

import Util.Util

import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.IRs
import Compiler.InstructionSelection
import Compiler.Legalize
import Compiler.RemovePhi
import Compiler.RegisterAlloc
import Compiler.CallingConvention
import Compiler.Globals
import Compiler.Stacking
import Compiler.Sparsity
import Compiler.RemoveLabels
import Compiler.Analysis

import MicroRAM (MWord)
import qualified MicroRAM as MRAM  (Program) 

(<.>) :: Monad m => (b -> c) -> (a -> b) -> a -> m c
f <.> g = \x -> return $ f $ g x 

compile :: Word -> LLVM.Module
        -> Hopefully $ CompilationUnit (MRAM.Program Name MWord)
compile len llvmProg = (return $ prog2unit len llvmProg)
  >>= (tagPass "Instruction Selection" $ justCompile instrSelect)
  >>= (tagPass "Legalize Instructions" $ justCompile legalize)
  >>= (tagPass "Edge split" $ justCompile edgeSplit)
  >>= (tagPass "Register Allocation" $ justCompile $ registerAlloc def)
  >>= (tagPass "Calling Convention" $ justCompile callingConvention)
  >>= (tagPass "Remove Globals" $ replaceGlobals)
  >>= (tagPass "Stacking" $ justCompile stacking)
  >>= (tagPass "Computing Sparsity" $ justAnalyse (SparsityData <.> sparsity))
  >>= (tagPass "Removing labels" $ justCompile removeLabels)
          
