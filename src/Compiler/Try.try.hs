{-# LANGUAGE OverloadedStrings #-}
module Compiler.Try () where

-- If you are seeing this, somehow my .gitignore failed. This file is for personal use.
-- I use this module as a testing ground, should not be taken seriously. Go back to where you came from.

import Compiler.Compiler
import LLVMutil.LLVMutil
import qualified LLVM.AST as LLVM
import qualified MicroRAM.MicroRAM as MRAM
import MicroRAM.MRAMInterpreter
import LLVM.AST (Named(..))
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.IntegerPredicate as IntPred
import GHC.Word as Word
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as Short
import qualified Data.String as String
import qualified LLVM.AST.Linkage
import qualified LLVM.AST.Visibility
import qualified LLVM.AST.CallingConvention

{-ex1 :: LLVM.Module
ex1 = fromBlocks bb1

ex1Comp = compile ex1
-- test1 n = (flip execute (n) <$> ex1Comp)
-}
label
  :: LLVM.Name
     -> [Named LLVM.Instruction]
     -> Named LLVM.Terminator
     -> LLVM.BasicBlock
label name insts term =
  LLVM.BasicBlock name insts term
  
-- # Example 2
{- fibonacci
-}
fib :: [LLVM.BasicBlock]
fib = [
  label "main" [ 
      "a":= ("a" .+ 1)]
    (Do $ LLVM.Br "loop" []),
    label "loop" [
      "b":= ("a" .+ 0),
      "a":= ("a" .+ ""),
      "":= ("b" .+ 0)]
    (Do $ LLVM.Br "loop" [])]

fibMain = fromBlocks fib
fibCompile = compile fibMain
   {-  
ex2Comp = codegenBlocks genv ex2
test2 n = (flip execute (4+n*4) <$> ex2Comp) -- returns fib n

-}
