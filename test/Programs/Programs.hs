{-# LANGUAGE PatternSynonyms #-}

module Programs.Programs (
  oneTest, allTests, riscvTests, TestProgram(..), TestGroupAbs (..),
  pattern OneLLVM, pattern OneRISCV
  ) where

import Compiler (Domain(..), DomainInput(..))
import MicroRAM


------------------
-- The Programs
------------------
testTrivial, testLoops, testGEP, testDatastruct, testBugs, testCorrectness, allTests, testErrors, riscvTests :: TestGroupAbs

oneTest = OneTest $ defaultTest {
    testName = "Heartbleed"
    , testDomains = OneLLVM "test/Programs/noni/heartbleed.ll"
    , testLen = 5000
    , testResult = 0
    , leakTainted = True
    , bug = True
    }

allTests = ManyTests "Program tests"
        [testCorrectness, testErrors, testBugs, testMulti]
testCorrectness = ManyTests "Correctness tests"
                  [testTrivial, testLoops, testGEP, testDatastruct, testFunctionPointer, testVarArgs, testCmov]

-- Trivial test, just to see the basics are working
testTrivial = ManyTests "Trivial programs" $ OneTest <$>
  defaultTest {
    testName = "Return 42"
    ,testDomains = OneLLVM "test/Programs/return42.ll"
    ,testLen = 30
    ,testResult = 42
    } :
  defaultTest {
    testName = "21 + 21"
    ,testDomains = OneLLVM "test/Programs/compute42.ll"
    ,testLen = 70
    ,testResult = 42
    }  :
  defaultTest {
    testName = "Constant Shr"
    ,testDomains = OneLLVM "test/Programs/constantShr.ll"
    ,testLen = 50
    ,testResult = 4 -- Unfortunately this depends on shape of memory, since it uses a ptrtoint
    }  :
  defaultTest {
    testName = "Trivial intrinsic call"
    ,testDomains = OneLLVM "test/Programs/intrinsicAdd.ll"
    ,testLen = 50
    ,testResult = 120
    }  :
  defaultTest {
    testName = "Function call with multiple args"
    ,testDomains = OneLLVM "test/Programs/multiArgs.ll"
    ,testLen = 180 -- 120
    ,testResult = 123
    } :
  defaultTest {
    testName = "Function call with 10 args"
    ,testDomains = OneLLVM "test/Programs/manyArgs.ll"
    ,testLen = 400
    ,testResult = 45
    } :
  defaultTest {
    testName = "arithmetic shift right"
    ,testDomains = OneLLVM "test/Programs/arithShr.ll"
    ,testLen = 1000 -- 850-- 660
    ,testResult = 8
    }  :
  defaultTest {
    testName = "load from global"
    ,testDomains = OneLLVM "test/Programs/trivial_global.ll"
    ,testLen = 50
    ,testResult =   10
    } :
  defaultTest {
    testName = "load from global array"
    ,testDomains = OneLLVM "test/Programs/trivial_global_array.ll"
    ,testLen = 50
    ,testResult =   10
    } :
  defaultTest {
    testName = "signed icmp"
    ,testDomains = OneLLVM "test/Programs/icmp_signed.ll"
    ,testLen = 100
    ,testResult = 3
    } :
  defaultTest {
    testName = "binops with truncated operands"
    ,testDomains = OneLLVM "test/Programs/binopTrunc.ll"
    ,testLen = 300
    ,testResult = 9
    } :
    []

-- Compilation errors
testErrors = ManyTests "Test errors" $ OneTest <$>
  defaultTest {
    testName = "Error for undefined functions"
    ,testDomains = OneLLVM "test/Programs/errorUndefinedFunction.ll"
    , compError = True
    } :
  []

testFunctionPointer = ManyTests "Test function pointers" $ OneTest <$> [
    defaultTest {
      testName = "call function pointer"
    , testDomains = OneLLVM "test/Programs/funcPointer.ll"
    , testLen = 200 -- 100
    , testResult = 5
    },
    defaultTest {
      testName = "function pointer in static initializer"
    , testDomains = OneLLVM "test/Programs/funcPointerStatic.ll"
    , testLen = 200 -- 100
    , testResult = 5
    },
    defaultTest {
      testName = "function pointer with multiple args"
    , testDomains = OneLLVM "test/Programs/funcPointerArgs.ll"
    , testLen = 150
    , testResult = 15
    }
  ]

testVarArgs = ManyTests "Test varargs" $ OneTest <$> [
    defaultTest {
      testName = "call function with varargs"
    , testDomains = OneLLVM "test/Programs/varArgs.ll"
    , testLen = 900 -- 700
    , testResult = 15
    }
  , defaultTest {
      testName = "call another function with varargs"
    , testDomains = OneLLVM "test/Programs/varArgs2.ll"
    , testLen = 2500 -- 2000
    , testResult = 23
    }
  , defaultTest {
      testName = "call a third function with varargs"
    , testDomains = OneLLVM "test/Programs/varArgs3.ll"
    , testLen = 2500 -- 2000
    , testResult = 30
    }
  ]

-- Register allocator needs to count the return register in Icmov as "read register " (i.e. it's live)
testCmov = ManyTests "Test cmov" $ OneTest <$> [
    defaultTest {
      testName = "Many cmov to test reg. alloc."
    , testDomains = OneLLVM "test/Programs/select.ll"
    , testLen = 200
    , testResult = 44
    }
  ]

-- Conditionals, Branching and loops

testLoops = ManyTests "Conditionals, Branching and loops" $ OneTest <$>
  defaultTest {
    testName = "Fibonacci loop (not optimized)"
    ,testDomains = OneLLVM "test/Programs/fibSlow.ll"
    ,testLen = 800
    ,testResult =   55
    } :
  defaultTest {
    testName = "Easy function call"
    ,testDomains = OneLLVM "test/Programs/easyFunction.ll"
    ,testLen = 100 -- 70
    ,testResult =   42
    } : 
  defaultTest {
    testName = "More easy function calls"
    ,testDomains = OneLLVM "test/Programs/callingConventions.ll"
    ,testLen = 100
    ,testResult =   42
    } : 
  defaultTest {
    testName = "Factorial with recursive calls"
    ,testDomains = OneLLVM "test/Programs/factRec.ll"
    ,testLen = 800 -- 600
    ,testResult =   120
    } :
  defaultTest {
    testName = "Or with phi"
    ,testDomains = OneLLVM "test/Programs/or.ll"
    ,testLen = 200 -- 150 -- 100
    ,testResult =   1
    } : 
  {-  defaultTest {
    testName = "Input text into numbers"
    ,testDomains = OneLLVM "test/Programs/returnInput.ll"
    ,testLen = 80
    ,testResult = 42
    } : -}
--  defaultTest { "Hello world" "test/Programs/hello.ll" 50 [] 0 :
    defaultTest {
    testName = "Memcpy"
    ,testDomains = OneLLVM "test/Programs/memcpy.ll"
    ,testLen = 300
    ,testResult = 123
    } :
    []

-- GetElementPtr
testGEP = ManyTests "Test structs and arrays with GetElementPtr" $ OneTest <$>
  defaultTest {
    testName = "Trivial array"
    ,testDomains = OneLLVM "test/Programs/easyArray.ll"
    ,testLen = 50
    ,testResult =   11
    } :
    defaultTest {
    testName = "Trivial struct"
    ,testDomains = OneLLVM "test/Programs/easyStruct.ll"
    ,testLen = 100 -- 50
    ,testResult =   3
    } :
    defaultTest {
    testName = "Trivial struct Packed"
    ,testDomains = OneLLVM "test/Programs/easyStructPack.ll"
    ,testLen = 200
    ,testResult =   3
    } :
{-  WAIT FOR FUNCTIONS TO WORK.
    defaultTest {
    testName = "Simple Binary Tree"
    ,"test/Programs/easyBinaryTree.ll"
    ,testLen = 100
    ,testResult = 42
    } :  -}
    defaultTest {
    testName = "Linked list length 3"
    ,testDomains = OneLLVM "test/Programs/easyLinkedList.ll"
    ,testLen = 240
    ,testResult = 16
    } :
    []
testDatastruct = ManyTests "Test data structures" $ OneTest <$>
  defaultTest {
  testName = "Linked list generic"
  ,testDomains = OneLLVM "test/Programs/LinkedList/linkedList.c.ll"
  ,testLen = 2500 -- 1500
  ,testResult = 42
  } :
  defaultTest {
  testName = "Binary search tree"
  ,testDomains = OneLLVM "test/Programs/binaryTree/binaryTree.c.ll"
  ,testLen = 7000 --3500
  ,testResult = 30
    } :
  []

-- ## Now we test buggy programs to see if we can catch the bug

testBugs = ManyTests "Compiler bug tests" $ OneTest <$>
  defaultTest {
  testName = "Use after free Bug"
  ,testDomains = OneLLVM "test/Programs/UseAfterFree/useAfterFree.c.ll"
  ,testLen = 250
  ,testResult = 0
  , bug = True } :
  defaultTest {
  testName = "Invalid Free (Now a pointer given by malloc)"
  ,testDomains = OneLLVM "test/Programs/WrongFree/wrongFree.c.ll"
  ,testLen = 250
  ,testResult = 0
  , bug = True }  :
  defaultTest {
  testName = "Out of bounds access"
  ,testDomains = OneLLVM "test/Programs/MallocOOB/mallocOOB.c.ll"
  ,testLen = 250
  ,testResult = 0
  , bug = True }  :
  defaultTest {
  testName = "Free after free"
  ,testDomains = OneLLVM "test/Programs/DoubleFree/DoubleFree.c.ll"
  ,testLen = 250
  ,testResult = 0
  , bug = True }  :
  defaultTest {
    testName = "Information leakage"
  , testDomains = OneLLVM "test/Programs/noni/explicit0.ll"
  , testLen = 200
  , testResult = 0
  , leakTainted = True
  , bug = True }  :
  defaultTest {
    testName = "Information leakage: heartbleed"
  , testDomains = OneLLVM "test/Programs/noni/simple_heartbleed_nobranch.ll" -- "test/Programs/noni/heartbleed.ll"
  , testLen = 7000 -- 6000 -- 5000
  , testResult = 0
  , leakTainted = True
  , bug = True }  :
  []


testMulti = ManyTests "Multi-input tests" $ OneTest <$>
  defaultTest
    { testName = "Link f and g (LLVM)"
    , testDomains = oneDomain
      [ InputLLVM "test/Programs/domains/g_main.ll" Nothing
      , InputLLVM "test/Programs/domains/f.ll" Nothing ]
    , testLen = 200
    , testResult = 203 } :
  defaultTest
    { testName = "Link f and g (RISC-V)"
    , testDomains = oneDomain
      [ InputRISCV "test/Programs/domains/g_main.s" Nothing
      , InputRISCV "test/Programs/domains/f.s" Nothing ]
    , testLen = 200
    , testResult = 203 } :
  []


-- ## Test the RISC-V backend

riscvTests = ManyTests "RISC-V backend correctness tests" $ OneTest <$>
  defaultTest {
  testName = "Compute 42"
  ,testDomains = OneRISCV "test/Programs/RISCV/compute42.s"
  ,testLen = 100
  ,testResult = 42} :
  defaultTest {
  testName = "Fibonacci"
  ,testDomains = OneRISCV "test/Programs/RISCV/fib.s"
  ,testLen = 500
  ,testResult = 55} :
  defaultTest {
  testName = "Square"
  ,testDomains = OneRISCV "test/Programs/RISCV/square.s"
  ,testLen = 500
  ,testResult = 64} : 
  defaultTest {
  testName = "Function Pointer"
  ,testDomains = OneRISCV "test/Programs/RISCV/funcPointer.s"
  ,testLen = 200
  ,testResult = 5} : 
  defaultTest {
  testName = "String Escape"
  ,testDomains = OneRISCV "test/Programs/RISCV/string_escape.s"
  ,testLen = 200
  ,testResult = 1} :
  defaultTest {
  testName = "String Escape Octal numbers"
  ,testDomains = OneRISCV "test/Programs/RISCV/string_escape_octal.s"
  ,testLen = 1500
  ,testResult = 1} :
  defaultTest {
  testName = "SRAW Instruction (Arithmetic Right-Shift)"
  ,testDomains = OneRISCV "test/Programs/RISCV/sraw.s"
  ,testLen = 500
  ,testResult = 1} :
  []

------------------
-- The constructs
------------------

data TestProgram =
  TestProgram {
    testName :: String
  , testDomains :: [Domain]
  , testLen :: Word         -- ^ How long to run it
  , compError :: Bool            -- ^ if compilation should throw an error
  , testResult :: MWord    -- ^ what it should return if it does (meaningless if bug)
  , bug :: Bool            -- ^ if the program has a bug we should find
  , leakTainted :: Bool    -- ^ Whether to run the program in "leak-tainted" mode
  } deriving (Eq, Show)

defaultTest :: TestProgram
defaultTest =  TestProgram {
  testName = ""
  , testDomains = []
  , testLen = 100
  , compError = False
  , testResult = 0
  , bug = False
  , leakTainted = False
  }

-- | the abstract version of a TestTree
data TestGroupAbs =
  OneTest TestProgram
  | ManyTests String [TestGroupAbs]
  deriving (Eq, Show)

pattern OneLLVM :: FilePath -> [Domain]
pattern OneLLVM path = [Domain
  { secretLengths = Nothing
  , privileged = False
  , domainInputs = [InputLLVM path Nothing]
  }]

pattern OneRISCV :: FilePath -> [Domain]
pattern OneRISCV path = [Domain
  { secretLengths = Nothing
  , privileged = False
  , domainInputs = [InputRISCV path Nothing]
  }]

oneDomain :: [DomainInput] -> [Domain]
oneDomain inputs = [Domain
  { secretLengths = Nothing
  , privileged = False
  , domainInputs = inputs
  }]


