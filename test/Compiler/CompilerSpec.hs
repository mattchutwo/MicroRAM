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

--import InstructionSelectionSpec


-- Compiler imports
import Compiler.CompileErrors
import Compiler.InstructionSelection
import Compiler.RegisterAlloc
import Compiler.Stacking
import Compiler.RemoveLabels


import LLVMutil.LLVMIO

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
main :: IO ()
main = defaultMain tests

tests = testGroup "Compiler tests" $
  map compileTest $
  [("Return 42", "Cprograms/return42.ll")
  ,("21 + 21", "Cprograms/compute42.ll")
  , ("Fibonacci loop (not optimized)", "Cprograms/fibSlow.ll")
  , ("Fibonacci loop", "Cprograms/fib.ll")
--  ,("Hello world", "Cprograms/hello.ll")
  ]

-- tests = testGroup "Compiler tests" [instructionSelectionTests]


-- Full compilation tests

-- | compileTest : compile step by step llvm code from file:

type AssertionInfo = IO String

compileTest ::
  (String, FilePath)
  -> TestTree
compileTest (testName, file) = 
  testCaseSteps ("Compiling " ++ testName) $ \step -> do
  step "Preparing code to compile"
  llvmModule <- llvmParse file
  step "Instruction selection "
  rtlModule <- checkPass $ instrSelect llvmModule
  step "Register Allocation "
  ltlModule <- checkPass $ trivialRegisterAlloc rtlModule
  step "Stacking "
  asmModule <- checkPass $ stacking ltlModule
  step "Removing Labels "
  checkPass $ removeLabels asmModule
  assertBool  "" True

checkPass :: Hopefully a -> IO a 
checkPass (Right c) = return c
checkPass (Left msg) = assertFailure $ "produced compilation error: " ++ (show msg)
