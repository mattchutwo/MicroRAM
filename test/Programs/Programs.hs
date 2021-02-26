module Programs.Programs (
  oneTest, allTests, TestProgram(..), TestGroupAbs (..)
  ) where

import MicroRAM


------------------
-- The Programs
------------------
testTrivial, testLoops, testGEP, testDatastruct, testBugs, testCorrectness, allTests, testErrors :: TestGroupAbs

oneTest = OneTest $ defaultTest {
  testName = "Linked list generic"
  ,fileName ="test/programs/LinkedList/linkedList.c.ll"
  ,testLen = 1500
  ,testResult = 42
  } 

allTests = ManyTests "Compiler tests"
        [testCorrectness, testErrors, testBugs]
testCorrectness = ManyTests "Compiler correctness tests"
                  [testTrivial, testLoops, testGEP, testDatastruct]

-- Trivial test, just to see the basics are working
testTrivial = ManyTests "Trivial programs" $ OneTest <$>
  defaultTest {
    testName = "Return 42"
    ,fileName = "test/programs/return42.ll"
    ,testLen = 30
    ,testResult = 42
    } :
  defaultTest {
    testName = "21 + 21"
    ,fileName ="test/programs/compute42.ll"
    ,testLen = 70
    ,testResult = 42
    }  :
  defaultTest {
    testName = "Trivial intrinsic call"
    ,fileName ="test/programs/intrinsicAdd.ll"
    ,testLen = 50
    ,testResult = 120
    }  :
  defaultTest {
    testName = "Function call with multiple args"
    ,fileName ="test/programs/multiArgs.ll"
    ,testLen = 110
    ,testResult = 123
    } :
  defaultTest {
    testName = "arithmetic shift right"
    ,fileName ="test/programs/arithShr.ll"
    ,testLen = 600
    ,testResult = 8
    }  :
  defaultTest {
    testName = "load from global"
    ,fileName ="test/programs/trivial_global.ll"
    ,testLen = 50
    ,testResult =   10
    } :
  defaultTest {
    testName = "load from global array"
    ,fileName ="test/programs/trivial_global_array.ll"
    ,testLen = 50
    ,testResult =   10
    } :
    []

-- Compilation errors
testErrors = ManyTests "Test errors" $ OneTest <$>
  defaultTest {
    testName = "Error for undefined functions"
    ,fileName = "test/programs/errorUndefinedFunction.ll"
    , compError = True
    } :
  []

-- Conditionals, Branching and loops

testLoops = ManyTests "Conditionals, Branching and loops" $ OneTest <$>
  defaultTest {
    testName = "Fibonacci loop (not optimized)"
    ,fileName ="test/programs/fibSlow.ll"
    ,testLen = 600
    ,testResult =   55
    } :
  defaultTest {
    testName = "Easy function call"
    ,fileName ="test/programs/easyFunction.ll"
    ,testLen = 70
    ,testResult =   42
    } : 
  defaultTest {
    testName = "More easy function calls"
    ,fileName ="test/programs/callingConventions.ll"
    ,testLen = 100
    ,testResult =   42
    } : 
  defaultTest {
    testName = "Factorial with recursive calls"
    ,fileName ="test/programs/factRec.ll"
    ,testLen = 600
    ,testResult =   120
    } :
  defaultTest {
    testName = "Or with phi"
    ,fileName ="test/programs/or.ll"
    ,testLen = 100
    ,testResult =   1
    } : 
  {-  defaultTest {
    testName = "Input text into numbers"
    ,fileName = "test/programs/returnInput.ll"
    ,testLen = 80
    ,testResult = 42
    } : -}
--  defaultTest { "Hello world" "test/programs/hello.ll" 50 [] 0 :
    []

-- GetElementPtr
testGEP = ManyTests "Test structs and arrays with GetElementPtr" $ OneTest <$>
  defaultTest {
    testName = "Trivial array"
    ,fileName ="test/programs/easyArray.ll"
    ,testLen = 50
    ,testResult =   11
    } :
    defaultTest {
    testName = "Trivial struct"
    ,fileName ="test/programs/easyStruct.ll"
    ,testLen = 50
    ,testResult =   3
    } :
    defaultTest {
    testName = "Trivial struct Packed"
    ,fileName ="test/programs/easyStructPack.ll"
    ,testLen = 200
    ,testResult =   3
    } :
{-  WAIT FOR FUNCTIONS TO WORK.
    defaultTest {
    testName = "Simple Binary Tree"
    ,"test/programs/easyBinaryTree.ll"
    ,testLen = 100
    ,testResult = 42
    } :  -}
    defaultTest {
    testName = "Linked list length 3"
    ,fileName ="test/programs/easyLinkedList.ll"
    ,testLen = 240
    ,testResult = 16
    } :
    []
testDatastruct = ManyTests "Test data structures" $ OneTest <$>
  defaultTest {
  testName = "Linked list generic"
  ,fileName ="test/programs/LinkedList/linkedList.c.ll"
  ,testLen = 1500
  ,testResult = 42
  } :
  defaultTest {
  testName = "Binary search tree"
  ,fileName ="test/programs/binaryTree/binaryTree.c.ll"
  ,testLen = 2400
  ,testResult = 30
    } :
  []

-- ## Now we test buggy programs to see if we can catch the bug

testBugs = ManyTests "Compiler bug tests" $ OneTest <$>
  defaultTest {
  testName = "Use after free Bug"
  ,fileName = "test/programs/UseAfterFree/useAfterFree.c.ll"
  ,testLen = 250
  ,testResult = 0
  , bug = True } :
  defaultTest {
  testName = "Invalid Free (Now a pointer given by malloc)"
  ,fileName = "test/programs/WrongFree/wrongFree.c.ll"
  ,testLen = 250
  ,testResult = 0
  , bug = True }  :
  defaultTest {
  testName = "Out of bounds access"
  ,fileName = "test/programs/MallocOOB/mallocOOB.c.ll"
  ,testLen = 250
  ,testResult = 0
  , bug = True }  :
  defaultTest {
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
  , compError :: Bool            -- ^ if compilation should throw an error
  , testResult :: MWord    -- ^ what it should return if it does (meaningless if bug)
  , bug :: Bool            -- ^ if the program has a bug we should find
  } deriving (Eq, Show)

defaultTest :: TestProgram
defaultTest =  TestProgram {
  testName = ""
  , fileName = ""
  , testLen = 100
  , compError = False
  , testResult = 0
  , bug = False   
  }

-- | the abstract version of a TestTree
data TestGroupAbs =
  OneTest TestProgram
  | ManyTests String [TestGroupAbs]
  deriving (Eq, Show)


