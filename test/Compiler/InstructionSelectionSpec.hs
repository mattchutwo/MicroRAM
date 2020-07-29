{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module InstructionSelectionSpec where

-- Standard imports
import GHC.Word as Word
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as Short
import qualified Data.String as String

--LLVM imports
import LLVMutil.LLVMutil
import qualified LLVM.AST as LLVM
import LLVM.AST (Named(..))
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.Linkage
import qualified LLVM.AST.Visibility
import qualified LLVM.AST.CallingConvention

-- Local Imports
import Compiler.IRs
import Compiler.Errors
import Compiler.InstructionSelection
import LLVMutil.LLVMIO

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck


instructionSelectionTests = testGroup
  "Instruction Selection (not correctness, just not failure)" isTests

{- This test suit, takes a list of LLVM files produced by clang and
   generates instructions for them.

   NOTE: The only thing we are checking is that compilation doesn't fail.
   Can't test correctness at this point.
-}

type AssertionInfo = IO String

checkCompiles :: Hopefully a -> AssertionInfo
checkCompiles (Right c) = return "Compiles"
checkCompiles (Left msg) = assertFailure $ "Produced error: " ++ (show msg)

claimCompiles ::  LLVM.Module -> AssertionInfo
claimCompiles llvmModule = checkCompiles $ instrSelect llvmModule

compileFromFile :: TestName -> FilePath -> TestTree
compileFromFile testName file = testCaseInfo testName $ do
  llvmModule <- llvmParse file
  claimCompiles llvmModule

isTests = [compileFromFile "Return 42" "programs/return42.ll",
          compileFromFile "21 + 21" "programs/compute42.ll"]
