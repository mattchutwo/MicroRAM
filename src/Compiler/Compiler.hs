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
import qualified LLVM.AST.Constant as LLVM.Constant
import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.List as List
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as Short
import qualified Data.Sequence as Seq (lookup, fromList)
import qualified Data.Word as Word

--import Compiler.CodeGenerator
--import Compiler.Assembler
import Compiler.CompileErrors
import Compiler.IRs
import Compiler.InstructionSelection
import Compiler.RegisterAlloc
import Compiler.Stacking
import Compiler.RemoveLabels

import qualified MicroRAM.MicroRAM as MRAM  (MAProgram,Program,NamedBlock(..)) 


compile :: LLVM.Module
        -> Hopefully $ (MRAM.Program Name Word)
compile llvmProg = (return llvmProg) >>=
  instrSelect >>= registerAlloc >>= stacking >>= removeLabels


{-
-- Old usefull piece of code.

compileStraight :: [LLVM.Named LLVM.Instruction]
        -> CgMonad (MicroRAM.MicroRAM.Program Int Word)
compileStraight llvmProg = do
  assCode <- codeGenStraight llvmProg
  mramProg <- assemble [NBlock Nothing $ assCode]
  Right mramProg
-}
