{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Compiler.CompilerSpec where

import MicroRAM.MRAMInterpreter
import MicroRAM (MWord)

-- Compiler imports
import Compiler
import Compiler.Errors

import LLVMutil.LLVMIO
import Test.Tasty

import qualified Test.QuickCheck.Monadic as QCM
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain testsWithOptions 

-- We can't generate inputs right now, so set test number to 1
testsWithOptions :: TestTree
testsWithOptions = localOption (QuickCheckTests 1) tests


{-tests' = testGroup "Compiler tests" $
        map (executionTest "Return argc" "test/programs/fibSlow.ll" [1,1,1,1,1,1,1,1,1,1]) $
        take 375 $ nats
nats :: [Int] 
nats = iterate ((+) 1) 0 -}

tests, testTrivial, testLoops, testGEP,testDatastruct, testCorrectness, testBugs, justOne :: TestTree
tests = testGroup "Compiler tests" $
        [testCorrectness, testBugs]


justOne = testGroup " Just one test" $
  compileCorrectTest
  "Linked list generic"
  "test/programs/LinkedList/linkedList.c.ll"
  1500 42 :
    []

-- # Test correctness of the compiler
testCorrectness = testGroup "Compiler correctness tests" $
                  [testTrivial, testLoops, testGEP, testDatastruct]

-- Trivial test, just to see the basics are working
testTrivial = testGroup "Trivial programs" $
  compileCorrectTest
    "Return 42"
    "test/programs/return42.ll"
    30 42 :
  compileCorrectTest
    "21 + 21"
    "test/programs/compute42.ll"
    70 42 :
  compileCorrectTest
    "Trivial intrinsic call"
    "test/programs/intrinsicAdd.ll"
    50 120 :
  compileCorrectTest
    "Function call with multiple args"
    "test/programs/multiArgs.ll"
    110 123 :
  compileCorrectTest
    "arithmetic shift right"
    "test/programs/arithShr.ll"
    600 8 :
  compileCorrectTest
    "load from global"
    "test/programs/trivial_global.ll"
    50 10 :
  compileCorrectTest
    "load from global array"
    "test/programs/trivial_global_array.ll"
    50 10 :
    []

-- Conditionals, Branching and loops
testLoops = testGroup "Conditionals, Branching and loops" $
  compileCorrectTest
    "Fibonacci loop (not optimized)"
    "test/programs/fibSlow.ll"
    600 55 :
  compileCorrectTest
    "Easy function call"
    "test/programs/easyFunction.ll"
    70 42 : 
  compileCorrectTest
    "More easy function calls"
    "test/programs/callingConventions.ll"
    100 42 : 
  compileCorrectTest
    "Factorial with recursive calls"
    "test/programs/factRec.ll"
    600 120 :
  compileCorrectTest
    "Or with phi"
    "test/programs/or.ll"
    100 1 : 
  {-  compileCorrectTest
    "Input text into numbers"
    "test/programs/returnInput.ll"
    80 42 : -}
--  compileCorrectTest "Hello world" "test/programs/hello.ll" 50 [] 0 :
    []

-- GetElementPtr
testGEP = testGroup "Test structs and arrays with GetElementPtr" $
  compileCorrectTest
    "Trivial array"
    "test/programs/easyArray.ll"
    50 11 :
    compileCorrectTest
    "Trivial struct"
    "test/programs/easyStruct.ll"
    50 3 :
    compileCorrectTest
    "Trivial struct Packed"
    "test/programs/easyStructPack.ll"
    200 3 :
{-  WAIT FOR FUNCTIONS TO WORK.
    compileCorrectTest
    "Simple Binary Tree"
    "test/programs/easyBinaryTree.ll"
    100 42 :  -}
    compileCorrectTest
    "Linked list length 3"
    "test/programs/easyLinkedList.ll"
    240 16 :
    []

testDatastruct = testGroup "Test data structures" $
  compileCorrectTest
  "Linked list generic"
  "test/programs/LinkedList/linkedList.c.ll"
  1500 42 :
  compileCorrectTest
  "Binary search tree"
  "test/programs/binaryTree/binaryTree.c.ll"
  2400 30 :
  []

-- ## Now we test buggy programs to see if we can catch the bug
testBugs = testGroup "Compiler bug tests" $
  compileBugTest
  "Use after free Bug"
  "test/programs/UseAfterFree/useAfterFree.c.ll"
  250 :
  compileBugTest
  "Invalid Free (Now a pointer given by malloc)"
  "test/programs/WrongFree/wrongFree.c.ll"
  250 :
  compileBugTest
  "Out of bounds access"
  "test/programs/MallocOOB/mallocOOB.c.ll"
  250 :
  compileBugTest
  "Free after free"
  "test/programs/DoubleFree/DoubleFree.c.ll"
  250 :
  []

  
  
-- tests = testGroup "Compiler tests" [instructionSelectionTests]


-- # Full compilation tests

compileTest
  :: Executor AReg t -- ^ executes the program and returns some result
  -> (t -> Bool)     -- ^ tests if result is satisfactory
  -> TestName
  -> FilePath
  -> Word
  -> TestTree
compileTest executionFunction tester name file len = 
  testProperty name $ 
  QCM.monadicIO $ do
  --QCM.run $ putStrLn "Here"
  answer <- QCM.run $ compileTest' file len  False
  --QCM.run $ putStrLn "Here 2" 
  QCM.assert $ tester answer
  where {-compileTest' ::
          FilePath
          -> Word -- ^ Length
          -> Bool -- ^ verbose
          -> IO MWord -- TestTree-}
        compileTest' file len _verb = do
          llvmProg <- llvmParse file
          mramProg <- handleErrorWith $ compile False len llvmProg Nothing 
          return $ executionFunction mramProg
  

-- ## Full compilation tests of correctness

-- | compileCorrectTest : compile step by step llvm code from file:

type AssertionInfo = IO String

compileCorrectTest ::
  String
  -> FilePath
  -> Word -- ^ Length
  -> MWord  -- ^ return value
  -> TestTree
compileCorrectTest name file len ret =
  compileTest (execAnswer False) (== ret) name file len

-- ## Full compilation tests looking for bugs

-- | compileCorrectTest : compile step by step llvm code from file:

compileBugTest ::
  String
  -> FilePath
  -> Word -- ^ Length
  -> TestTree
compileBugTest name file len = 
  compileTest (execBug False) id name file len



