{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Compiler.CompilerSpec where

import MicroRAM.MRAMInterpreter
import MicroRAM.MicroRAM (MWord)

-- Compiler imports
import Compiler.Errors
import Compiler.Compiler

import LLVMutil.LLVMIO
import Test.Tasty
import qualified Test.QuickCheck.Monadic as QCM
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

{-tests' = testGroup "Compiler tests" $
        map (executionTest "Return argc" "test/programs/fibSlow.ll" [1,1,1,1,1,1,1,1,1,1]) $
        take 375 $ nats
nats :: [Int] 
nats = iterate ((+) 1) 0 -}

tests, testTrivial, testLoops, testGEP :: TestTree
tests = testGroup "Compiler tests" $
        [testTrivial, testLoops, testGEP]

    
-- Trivial test, just to see the basics are working
testTrivial = testGroup "Trivial programs" $
  compileTest
    "Return 42"
    "test/programs/return42.ll"
    25 42 :
  compileTest
    "21 + 21"
    "test/programs/compute42.ll"
    70 42 :
    []

-- Conditionals, Branching and loops
testLoops = testGroup "Conditionals, Branching and loops" $
  compileTest
    "Fibonacci loop (not optimized)"
    "test/programs/fibSlow.ll"
    420 34 : 
  compileTest
    "Fibonacci loop"
    "test/programs/fib.ll"
    420 34 :
    []

-- GetElementPtr
testGEP = testGroup "Test structs and arrays with GetElementPtr" $
  compileTest
    "Trivial array"
    "test/programs/easyArray.ll"
    50 11 :
    compileTest
    "Trivial struct"
    "test/programs/easyStruct.ll"
    50 3 :
{-  WAIT FOR FUNCTIONS TO WORK.
    compileTest
    "Simple Binary Tree"
    "test/programs/easyBinaryTree.ll"
    100 42 :  -}
    compileTest
    "Linked list length 3"
    "test/programs/easyLinkedList.ll"
    140 16 :
    []
    

-- tests = testGroup "Compiler tests" [instructionSelectionTests]


-- Full compilation tests

-- | compileTest : compile step by step llvm code from file:

type AssertionInfo = IO String

compileTest ::
  String
  -> FilePath
  -> Word -- ^ Length
  -> MWord  -- ^ return value
  -> TestTree
compileTest name file len ret = 
  testProperty name $ 
  QCM.monadicIO $ do
  --QCM.run $ putStrLn "Here"
  answer <- QCM.run $ compileTest' file len  False
  --QCM.run $ putStrLn "Here 2" 
  QCM.assert $ answer == ret

compileTest' ::
  FilePath
  -> Word -- ^ Length
  -> Bool -- ^ verbose
  -> IO MWord -- TestTree
compileTest' file len _verb = do
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
