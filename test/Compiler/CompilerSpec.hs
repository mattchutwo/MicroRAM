{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Compiler.CompilerSpec where

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
--import Data.Bits.Extras
--import Data.Sequence as Seq

--import qualified Test.QuickCheck as QC



import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
main :: IO ()
main = defaultMain tests

tests = testGroup "Compiler tests" []

  
-- ** Definitinos
  
-- ## Blocks
label
  :: LLVM.Name
     -> [Named LLVM.Instruction]
     -> Named LLVM.Terminator
     -> LLVM.BasicBlock
label name insts term =
  LLVM.BasicBlock name insts term

-- # Example 0
{- Compute x^2 + 3x - 10 (For now x = 42)
-}
bb0 :: [LLVM.BasicBlock]
bb0 = [
  label "main0" [
      "a":=  ("a" .+ 42)     -- r0 = 42
      ,"c":= ("a" .* "a")    -- r2 = r0 * r0
      ,"b":= ("a" .* 3)      -- r1 = 3 * r0
      ,"a":= ("b" .+ "c")    -- r0 = r1 + r2
      ,"a":= ("a" .- 10)     -- r0 = r0 - 10
      ]
    (Do $ LLVM.Ret Nothing [])]

ex0 :: LLVM.Module
ex0 = fromBlocks bb0

ex0Comp = compile ex0

-- # Example 1
{- Adds 1 to "a" forever
-}
bb1 :: [LLVM.BasicBlock]
bb1 = [
  label "main" [
      "a":= ("a" .+ 1)]
    (Do $ LLVM.Br "loop" []),
    label "loop" [
      "a":= ("a" .+ 1),
      "":= ("a" .+ 0)]
    (Do $ LLVM.Br "loop" [])]

ex1 :: LLVM.Module
ex1 = fromBlocks bb1

ex1Comp = compile ex1
-- test1 n = (flip execute (n) <$> ex1Comp)-}


-- # Example 2
{- fibonacci

ex2 :: [LLVM.BasicBlock]
ex2 = [
  label "main" [ 
      "a":= ("a" .+ 1)]
    (Do $ LLVM.Br "loop" []),
    label "loop" [
      "b":= ("a" .+ 0),
      "a":= ("a" .+ ""),
      "":= ("b" .+ 0)]
    (Do $ LLVM.Br "loop" [])]
     
ex2Comp = codegenBlocks genv ex2
test2 n = (flip execute (4+n*4) <$> ex2Comp) -- returns fib n
-}
