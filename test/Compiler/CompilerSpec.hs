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

import qualified Data.Map as Map

--import InstructionSelectionSpec


-- Compiler imports
import Compiler.Registers
import Compiler.CompileErrors
import Compiler.InstructionSelection
import Compiler.RegisterAlloc
import Compiler.Stacking
import Compiler.RemoveLabels
import Compiler.IRs


import LLVMutil.LLVMIO

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
main :: IO ()
main = defaultMain tests

tests' = testGroup "Compiler tests" $
        map (executionTest "Return argc" "programs/fibSlow.ll" [1,1,1,1,1,1,1,1,1,1]) $
        take 375 $ nats
nats :: [Int] 
nats = iterate ((+) 1) 0
        
  
tests = testGroup "Compiler tests" $
  compileTest
    "Return 42"
    "programs/return42.ll"
    25 [] 42 :
  compileTest
    "21 + 21"
    "programs/compute42.ll"
    70 [] 42 :
  compileTest
    "Return argc"
    "programs/returnArgc.ll"
    50 [1,2,3] 3 :
  compileTest
    "Fibonacci loop (not optimized)"
    "programs/fibSlow.ll"
    375 (take 10 $ repeat 1) 34 :
  compileTest
    "Fibonacci loop"
    "programs/fibCheeky.ll"
    200 [1,2,3,4] 2 :
  compileTest
    "Fibonacci loop"
    "programs/fib.ll"
    100 [1,2,3,4] 2 :
--  compileTest "Hello world" "programs/hello.ll" 50 [] 0 :
    []

-- tests = testGroup "Compiler tests" [instructionSelectionTests]


-- Full compilation tests

-- | compileTest : compile step by step llvm code from file:

type AssertionInfo = IO String

compileTest ::
  String
  -> FilePath
  -> Int   -- ^ runtime / fuel
  -> [Word]  -- ^ Input
  -> Word  -- ^ return value
  -> TestTree
compileTest testName file bound input answer = 
  testCaseSteps ("Compiling: " ++ testName) $ \step -> do
  step "Preparing code to compile"
  llvmModule <- llvmParse file
  --step $ show llvmModule
  step "Instruction selection "
  rtlModule <- checkPass $ instrSelect llvmModule
  --step $ show rtlModule
  step "Register Allocation "
  ltlModule <- checkPass $ trivialRegisterAlloc rtlModule
  --step $ show ltlModule
  step "Stacking "
  asmModule <- checkPass $ stacking ltlModule
  --step $ show asmModule 
  step "Removing Labels "
  mram <- checkPass $ removeLabels asmModule
  --step $ show mram
  step "Testing correctness" 
  assertEqual "Program returned the wrong value." answer  (exec mram bound input)


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
  ltlModule <- checkPass $ trivialRegisterAlloc rtlModule
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
  step $ show (exec_tape mram (bound) input) 
  --step $ show (exec_flag mram (bound) input)
  step $ "Return at step " ++ show bound ++ ": " ++ show (exec mram bound input)
  assertEqual "Program returned the wrong value." 0 0




  
checkPass :: Hopefully a -> IO a 
checkPass (Right c) = return c
checkPass (Left msg) = assertFailure $ "produced compilation error: " ++ (show msg)

exec :: Regs mreg => MRAM.Program mreg Word -> Int -> [Word] -> Word
exec prog bound input = answer $ (run input [] prog) !! bound

rmsnd (RMap x m) = m

exec_regs :: MRAM.Program Name Word -> Int -> [Word] -> [(Name, Word)]
exec_regs prog bound input = Map.toList $ rmsnd $ regs $ (run input [] prog) !! bound

exec_mem :: MRAM.Program Name Word -> Int -> [Word] -> [(Word, Word)]
exec_mem prog bound input = Map.toList $ snd $ mem $ (run input [] prog) !! bound

exec_pc prog bound input = pc $ (run input [] prog) !! bound

exec_tape prog bound input = tapes $ (run input [] prog) !! bound

exec_flag prog bound input = flag $ (run input [] prog) !! bound


pretyPrint step mram = mapM (\(n,inst) -> step $ (show n) ++ ". " ++ (show inst)) $ enumerate mram
enumerate ls = zip nats ls
