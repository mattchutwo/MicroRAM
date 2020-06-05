{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : LLVMexamples
Description : Bunch of "LLVM" examples. All of them should be also stored as
              text files in the 'llvm-examples' folder. The constructions here
              are easier to edit though.
Maintainer  : santiago@galois.com
Stability   : experimental


-}

module LLVMutil.LLVMexamples () where

import qualified LLVM.AST as LLVM 
import qualified LLVM.AST.Constant as LLVM.Constant
import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.List as List
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as Short
import qualified Data.Sequence as Seq (lookup, fromList)
import qualified Data.Word as Word

import MicroRAM.MicroRAM (Operand'(..), MAOperand) 
import qualified MicroRAM.MicroRAM as MRAM
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.ParameterAttribute as ParamAtt

import LLVM.AST (Named(..))
import LLVMutil.LLVMutil


-- ** Definitinos
  
-- ## Blocks
label
  :: LLVM.Name
     -> [LLVM.Named LLVM.Instruction]
     -> LLVM.Named LLVM.Terminator
     -> LLVM.BasicBlock
label name insts term =
  LLVM.BasicBlock name insts term

-- # Example 0
{- Compute x^2 + 3x - 10 (For now x = 42) -}

ex0 :: [LLVM.BasicBlock]
ex0 = [
  label "main0" [
      "a":=  ("a" .+ 42)     -- r0 = 42
      ,"c":= ("a" .* "a")    -- r2 = r0 * r0
      ,"b":= ("a" .* 3)      -- r1 = 3 * r0
      ,"a":= ("b" .+ "c")    -- r0 = r1 + r2
      ,"a":= ("a" .- 10)     -- r0 = r0 - 10
      ]
    (Do $ LLVM.Ret Nothing [])]

-- Looping fibonacci
{- Program calculates fibonacci numbers,
  using a loop.

  Sill not accepting inputs. So the input is hardcoded.
-}
--fibInit

fib :: [LLVM.BasicBlock]


fib = [
  label "main0" [
      "a":=  ("a" .+ 42)     -- r0 = 42
      ,"c":= ("a" .* "a")    -- r2 = r0 * r0
      ,"b":= ("a" .* 3)      -- r1 = 3 * r0
      ,"a":= ("b" .+ "c")    -- r0 = r1 + r2
      ,"a":= ("a" .- 10)     -- r0 = r0 - 10
      ]
    (Do $ LLVM.Ret Nothing [])]

main = putStrLn $ show fib
