{-# LANGUAGE TypeOperators #-}


{-|
Module      : Compiler
Description : LLVM -> MicroRAM
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

* LLVM: The LLVM assembly language as defined in <https://llvm.org/docs/LangRef.html>. We
  use the haskell implementation of `llvm-hs`.

* MicroIR: High-level IR based on MicroRAM.  Includes MicroRAM instructions with
   support for extended operand kinds and two non-register operands per
   instruction (normal MicroRAM requires one operand to be a register), as well
   as extended high-level instructions (see `MIRprog`).

* RTL: Register Transfer language (RTL) uses infinite virtual registers,
  function calls, Stack allocation and regular MicroRAM instructions (see `Rprog`). 

* LTL: Location Transfer Language (LTL) is close to RTL but uses machine registers
  and stack slots instead of virtual registers (see `Lprog`). 

* MicroASM: 

* MicroRAM


-}
module Compiler.Compiler
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
import Compiler.RegisterAlloc
import Compiler.Globals
import Compiler.Stacking
import Compiler.Sparsity
import Compiler.RemoveLabels
import Compiler.Analysis

import MicroRAM.MicroRAM (MWord)
import qualified MicroRAM.MicroRAM as MRAM  (Program) 

(<.>) :: Monad m => (b -> c) -> (a -> b) -> a -> m c
f <.> g = \x -> return $ f $ g x 

compile :: Word -> LLVM.Module
        -> Hopefully $ CompilationUnit (MRAM.Program Name MWord)
compile len llvmProg = (return $ prog2unit len llvmProg)
  >>= (tagPass "Instruction Selection" $ justCompile instrSelect)
  >>= (tagPass "Legalize Instructions" $ justCompile legalize)
  >>= (tagPass "Register Allocation" $ justCompile $ registerAlloc def)
  >>= (tagPass "Remove Globals" $ replaceGlobals)
  >>= (tagPass "Stacking" $ justCompile stacking)
  >>= (tagPass "Computing Sparsity" $ justAnalyse (SparsityData <.> sparsity))
  >>= (tagPass "Removing labels" $ justCompile removeLabels)
          
