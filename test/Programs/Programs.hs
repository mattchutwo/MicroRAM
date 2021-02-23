module Programs.Programs (
  allTests,
  TestProgram(..), TestGroup (..)
  ) where

import MicroRAM


------------------
-- The Programs
------------------
testTrivial, testLoops, testGEP, testDatastruct, testBugs, testCorrectness, allTests :: TestGroup

allTests = ManyTests "Compiler tests"
        [testCorrectness, testBugs]
testCorrectness = ManyTests "Compiler correctness tests"
                  [testTrivial, testLoops, testGEP, testDatastruct]

-- Trivial test, just to see the basics are working
testTrivial = ManyTests "Trivial programs" $ OneTest <$>
  TestProgram {
    testName = "Return 42"
    ,fileName = "test/programs/return42.ll"
    ,testLen = 30
    ,testResult = 42
    , bug = False} :
  TestProgram {
    testName = "21 + 21"
    ,fileName ="test/programs/compute42.ll"
    ,testLen = 70
    ,testResult = 42
    , bug = False}  :
  TestProgram {
    testName = "Trivial intrinsic call"
    ,fileName ="test/programs/intrinsicAdd.ll"
    ,testLen = 50
    ,testResult = 120
    , bug = False}  :
  TestProgram {
    testName = "Function call with multiple args"
    ,fileName ="test/programs/multiArgs.ll"
    ,testLen = 110
    ,testResult = 123
    , bug = False} :
  TestProgram {
    testName = "arithmetic shift right"
    ,fileName ="test/programs/arithShr.ll"
    ,testLen = 600
    ,testResult = 8
    , bug = False}  :
  TestProgram {
    testName = "load from global"
    ,fileName ="test/programs/trivial_global.ll"
    ,testLen = 50
    ,testResult =   10
    , bug = False} :
  TestProgram {
    testName = "load from global array"
    ,fileName ="test/programs/trivial_global_array.ll"
    ,testLen = 50
    ,testResult =   10
    , bug = False} :
    []

-- Conditionals, Branching and loops

testLoops = ManyTests "Conditionals, Branching and loops" $ OneTest <$>
  TestProgram {
    testName = "Fibonacci loop (not optimized)"
    ,fileName ="test/programs/fibSlow.ll"
    ,testLen = 600
    ,testResult =   55
    , bug = False} :
  TestProgram {
    testName = "Easy function call"
    ,fileName ="test/programs/easyFunction.ll"
    ,testLen = 70
    ,testResult =   42
    , bug = False} : 
  TestProgram {
    testName = "More easy function calls"
    ,fileName ="test/programs/callingConventions.ll"
    ,testLen = 100
    ,testResult =   42
    , bug = False} : 
  TestProgram {
    testName = "Factorial with recursive calls"
    ,fileName ="test/programs/factRec.ll"
    ,testLen = 600
    ,testResult =   120
    , bug = False} :
  TestProgram {
    testName = "Or with phi"
    ,fileName ="test/programs/or.ll"
    ,testLen = 100
    ,testResult =   1
    , bug = False} : 
  {-  TestProgram {
    testName = "Input text into numbers"
    ,fileName = "test/programs/returnInput.ll"
    ,testLen = 80
    ,testResult = 42
    , bug = False} : -}
--  TestProgram { "Hello world" "test/programs/hello.ll" 50 [] 0 :
    []

-- GetElementPtr
testGEP = ManyTests "Test structs and arrays with GetElementPtr" $ OneTest <$>
  TestProgram {
    testName = "Trivial array"
    ,fileName ="test/programs/easyArray.ll"
    ,testLen = 50
    ,testResult =   11
    , bug = False} :
    TestProgram {
    testName = "Trivial struct"
    ,fileName ="test/programs/easyStruct.ll"
    ,testLen = 50
    ,testResult =   3
    , bug = False} :
    TestProgram {
    testName = "Trivial struct Packed"
    ,fileName ="test/programs/easyStructPack.ll"
    ,testLen = 200
    ,testResult =   3
    , bug = False} :
{-  WAIT FOR FUNCTIONS TO WORK.
    TestProgram {
    testName = "Simple Binary Tree"
    ,"test/programs/easyBinaryTree.ll"
    ,testLen = 100
    ,testResult = 42
    , bug = False} :  -}
    TestProgram {
    testName = "Linked list length 3"
    ,fileName ="test/programs/easyLinkedList.ll"
    ,testLen = 240
    ,testResult = 16
    , bug = False} :
    []
testDatastruct = ManyTests "Test data structures" $ OneTest <$>
  TestProgram {
  testName = "Linked list generic"
  ,fileName ="test/programs/LinkedList/linkedList.c.ll"
  ,testLen = 1500
  ,testResult = 42
  , bug = False} :
  TestProgram {
  testName = "Binary search tree"
  ,fileName ="test/programs/binaryTree/binaryTree.c.ll"
  ,testLen = 2400
  ,testResult = 30
    , bug = False} :
  []

-- ## Now we test buggy programs to see if we can catch the bug

testBugs = ManyTests "Compiler bug tests" $ OneTest <$>
  TestProgram {
  testName = "Use after free Bug"
  ,fileName = "test/programs/UseAfterFree/useAfterFree.c.ll"
  ,testLen = 250
  ,testResult = 0
  , bug = True } :
  TestProgram {
  testName = "Invalid Free (Now a pointer given by malloc)"
  ,fileName = "test/programs/WrongFree/wrongFree.c.ll"
  ,testLen = 250
  ,testResult = 0
  , bug = True }  :
  TestProgram {
  testName = "Out of bounds access"
  ,fileName = "test/programs/MallocOOB/mallocOOB.c.ll"
  ,testLen = 250
  ,testResult = 0
  , bug = True }  :
  TestProgram {
  testName = "Free after free"
  ,fileName = "test/programs/DoubleFree/DoubleFree.c.ll"
  ,testLen = 250
  ,testResult = 0
  , bug = True }  :
  []



------------------
-- The constructs
------------------

data TestProgram =
  TestProgram {
  testName :: String
  , fileName :: FilePath
  , testLen :: Word         -- ^ How long to run it
  , testResult :: MWord    -- ^ what it should return if it does (meaningless if bug)
  , bug :: Bool            -- ^ if the program has a bug we should find
  } deriving (Eq, Show)


-- | the abstract version of a TestTree
data TestGroup =
  OneTest TestProgram
  | ManyTests String [TestGroup]
  deriving (Eq, Show)


