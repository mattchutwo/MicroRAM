{-# LANGUAGE TypeOperators #-}


{-|
Module      : Compiler
Description : LLVM -> MicroRAM
Maintainer  : santiago@galois.com
Stability   : prototype

This module compiles LLVM to MicroRAM. The compiler has the following
passes/IRs:

  
    +---------+
    |  LLVM   |
    +---------+
         | Instruction selection
    +----v----+
    |   RTL   |
    +---------+
         | Register allocation
    +----v----+
    |   LTL   |
    +---------+
         | Globals in memory
    +----v----+
    |   LTL   |
    +---------+
         | Stacking
    +----v----+
    |   Asm   |
    +---------+
         | Label removal
    +----v----+
    |MicroRAM |
    +---------+

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
          
