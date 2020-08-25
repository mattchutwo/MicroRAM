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
import           Data.Default
import qualified Data.String as String
import qualified LLVM.AST.Linkage
import qualified LLVM.AST.Visibility
import qualified LLVM.AST.CallingConvention

import qualified Data.Map as Map

--import InstructionSelectionSpec


-- Compiler imports
import Compiler.Registers
import Compiler.Errors
import Compiler.InstructionSelection
import Compiler.RegisterAlloc
import Compiler.Stacking
import Compiler.RemoveLabels
import Compiler.IRs


import LLVMutil.LLVMIO

import Test.Tasty
import Test.Tasty.Options
--import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Monadic as QCM
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

{-tests' = testGroup "Compiler tests" $
        map (executionTest "Return argc" "test/programs/fibSlow.ll" [1,1,1,1,1,1,1,1,1,1]) $
        take 375 $ nats
nats :: [Int] 
nats = iterate ((+) 1) 0 -}

tests = testGroup "Compiler tests" $
  compileTest
    "Return 42"
    "test/programs/return42.ll"
    25 42 :
  compileTest
    "21 + 21"
    "test/programs/compute42.ll"
    70 42 :
  compileTest
    "Fibonacci loop (not optimized)"
    "test/programs/fibSlow.ll"
    420 34 : 
  compileTest
    "Easy function call"
    "test/programs/easyFunction.ll"
    60 42 : 
  compileTest
    "More easy function calls"
    "test/programs/callingConventions.ll"
    70 42 : 
  compileTest
    "Fibonacci with recursive calls"
    "test/programs/fibRec.ll"
    600 34 :
  {-  compileTest
    "Input text into numbers"
    "test/programs/returnInput.ll"
    80 42 : -}
--  compileTest "Hello world" "test/programs/hello.ll" 50 [] 0 :
    []

-- tests = testGroup "Compiler tests" [instructionSelectionTests]


-- Full compilation tests

-- | compileTest : compile step by step llvm code from file:

type AssertionInfo = IO String

compileTest ::
  String
  -> FilePath
  -> Word -- ^ Length
  -> Word  -- ^ return value
  -> TestTree
compileTest name file len ret = 
  testProperty name $ 
  QCM.monadicIO $ do
  answer <- QCM.run $ compileTest' file len  False 
  QCM.assert $ answer == ret

compileTest' ::
  FilePath
  -> Word -- ^ Length
  -> Bool -- ^ verbose
  -> IO Word -- TestTree
compileTest' file len verb= do
  llvmProg <- llvmParse file
  mramProg <- handleErrorWith $ compile len llvmProg
  return $ execAnswer mramProg






{-    llvmModule <- llvmParse file
  


  do
  putStrLn "Preparing code to compile"
  llvmModule <- llvmParse file
  --putStrLn $ show llvmModule
  putStrLn "Instruction selection "
  rtlModule <- checkPass $ instrSelect llvmModule
  --step $ show rtlModule
  step "Register Allocation "
  ltlModule <- checkPass $ registerAlloc def rtlModule
  --step $ show ltlModule
  step "Stacking "
  asmModule <- checkPass $ stacking ltlModule
  --putStrLn $ show asmModule 
  putStrLn "Removing Labels"
  mram <- checkPass $ removeLabels asmModule
  --putStrLn $ show mram
  putStrLn "Testing correctness"
  return $ execAnswer mram -}

{-
executionTest ::
  String
  -> FilePath
  -> [Word]
  -> Int
  -> TestTree
executionTest testName file input bound = 
  testCaseSteps ("Compiling " ++ testName) $ \step -> do
  --step "Preparing code to compile"
  llvmModule <- llvmParse file
  --step $ show llvmModule
  --step "Instruction selection "
  rtlModule <- checkPass $ instrSelect llvmModule
  --step $ show rtlModule
  --step "Register Allocation "
  ltlModule <- checkPass $ registerAlloc def rtlModule
  --step $ show ltlModule
  --step "Stacking "
  asmModule <- checkPass $ stacking ltlModule
  --step $ show asmModule 
  --step "Removing Labels "
  mram <- checkPass $ removeLabels asmModule
  --pretyPrint step mram
  step $ show (exec_pc mram (bound) input) 
  step $ show (exec_regs mram (bound) input) 
  step $ show (exec_mem mram (bound) input)  
  -- step $ show (exec_tape mram (bound) input) 
  --step $ show (exec_flag mram (bound) input)
  step $ "Return at step " ++ show bound ++ ": " ++ show (exec mram bound input)
  assertEqual "Program returned the wrong value." 0 0


-}

  
--checkPass :: Hopefully a -> IO a 
--checkPass (Right c) = return c
--checkPass (Left msg) = assertFailure $ "produced compilation error: " ++ (show msg)

--exec :: Regs mreg => MRAM.Program mreg Word -> Int -> [Word] -> Word
--exec prog bound input = answer $ (run input prog) !! bound

--exec_tape prog bound input = tapes $ (run input prog) !! bound


-- pretyPrint step mram = mapM (\(n,inst) -> step $ (show n) ++ ". " ++ (show inst)) $ enumerate mram
