{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module MicroRAM.InterpreterSpec (main) where


import Control.Lens ((&), (%~))
import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck.Property as Prop (succeeded, failed, reason, Result)

import Compiler.Common (firstUnusedName)
import Compiler.Registers
import Compiler.CompilationUnit
import Compiler.Tainted

import Data.Bits
import Data.Default
import qualified Data.Vector as Vec

import MicroRAM
import MicroRAM.MRAMInterpreter
import MicroRAM.MRAMInterpreter.AbsInt (AbsValue(..))
import qualified MicroRAM.MRAMInterpreter.AbsInt as Abs




main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing the Interpreter for  MicroRAM"
  [test1,test2,test3,test4,test5, testAshr, testAbsMem]

-- * Pretty printers
_ppList :: Show a => [a] -> IO ()
_ppList all =  putStr $ concat $ map (\st -> show st ++ "\n") all

-- * Build a Compilation unit from a simpl program snip

-- | Build initial memory from a list of words

-- Old strategy
list2InitMem :: [MWord] -> InitialMem
list2InitMem ls = map word2InitSeg $ zip [0..] ls 
  where word2InitSeg :: (MWord,MWord) -> InitMemSegment
        word2InitSeg (loc,val) = InitMemSegment "list2Initmem" False False False loc 1 (Just [val]) (Just [bottomWord])



trivialCU :: Prog Int -> Word -> [MWord] -> CompilationResult (Prog Int)
trivialCU prog len input = CompUnit progs len InfinityRegs def firstUnusedName ()
  where pm = ProgAndMem prog (list2InitMem input) mempty
        progs = MultiProg pm pm

runProg :: Bool -> Prog Int -> Word -> [MWord] -> Trace Int
runProg leakTainted prog len input = run leakTainted $ trivialCU prog len input

-- We are treating the first register as the return
-- To get ouptu to get output provide a program and a number of steps to get the result after that long
-- Execute gets the trace and then looks at the first register after t steps
exec :: Bool -> Prog Int -> Word -> [MWord] -> MWord
exec leakTainted prog steps input = lookupReg sp (seeRegs (runProg leakTainted prog (steps+1) input) (fromEnum steps)) -- this throws an error if there is no register 0
simpl_exec :: Prog Int -> Word ->  MWord
simpl_exec prog steps = exec False prog steps [] -- when there are no inputs
seeRegs:: Trace mreg -> Int -> RegBank mreg MWord
seeRegs t n = regs (t !! n)

-- The tester setup
type Reg = Int

-- # Test 1
{- (1+2)*(3+4) = 21
-}

prog1 :: Program Reg MWord
prog1 = [Iadd 0 0 (Const 1),
       Iadd 0 0 (Const 2),
       Iadd 1 1 (Const 3),
       Iadd 1 1 (Const 4),
       Imull 0 0 (Reg 1)]

test1 :: TestTree
test1 = testProperty ("Testing (1+2)*(3+4) == 21.")  $ (simpl_exec prog1 5) == 21



-- # Test 2
{-
   while true {
     x++
   } 
-}

prog2 :: Program Reg MWord
prog2 = [Iadd 0 0 (Const 1),
       Ijmp (Const 0)]

test2 :: TestTree
test2 = testProperty "Test `x++` on a loop" $ \n -> (n :: Word) >= 1 ==>
                                                    simpl_exec prog2 (2*n) == fromIntegral n

-- # Test 3: fibonacci
{-
   x = 1
   while true {
     z = x
     x = x + y
     y = z
   } 
-}

fib_pure :: Word -> MWord
fib_pure 0 = 0
fib_pure 1 = 1
fib_pure n = fib_pure (n-1) + fib_pure (n-2) 

prog3 :: Program Reg MWord
prog3 = [Iadd 0 1 (Const 1), -- x=1
         Iadd 2 0 (Const 0), -- z=x
         Iadd 0 0 (Reg 1),  -- x=x+y
         Iadd 1 2 (Const 0),
       Ijmp (Const 1)]

_fibs:: [MWord]
_fibs = 0 : 1 : Prelude.zipWith (+) _fibs (tail _fibs)
_fib::Int -> MWord
_fib n = _fibs !! n

claimEqual  :: (Eq a, Show a) => a -> a -> Prop.Result
claimEqual a b = 
   if a == b
     then Prop.succeeded
     else Prop.failed { Prop.reason = "Got " ++ show a ++ " but expected " ++ show b}

test3 :: TestTree
test3 = testProperty "Test fibonacci" $ \n -> (n :: Word) <= 30 ==>
                                              (simpl_exec prog3 (1+4*n)) ==  fib_pure (n+1)
   
-- # Test 4: conditional + input

prog4 :: Program Reg MWord
prog4 = [IloadW 1 (Const 0), --
         Icmpa 3 1 (Const 10), -- 1
         Icjmp 3 (Const 5),    -- 2 
         Iadd 2 2 (Const 77),-- 3
         Ijmp (Const 6),     -- 4
         Iadd 2 2 (Const 42) -- Label: 5
        ,Imov 0 (Reg 2)      -- Label: 6
        , Ijmp (Const 6)     -- Label: 7
        ]

test4 :: TestTree
test4 = testProperty "Test a conditional and input" $ \x ->
   claimEqual (exec False prog4 6 [x]) (if x>10 then 42 else 77)
                                                               
-- # Test 5: sum all input
  {- for i in input
        x =+ i

NOTE: the initial memory contains, the size of the initial memory.
      so we start at mem=2            
-}


prog5 :: Program Reg MWord
{- Old verison with tapes:
prog5 = [Iread 1 (Const 0), Iadd 0 0 (Reg 1), Icjmp (Const 4), Ijmp (Const 0), Ijmp (Const 4)]
-}
-- New version with input in initial memory.
prog5 = [Imov 0 (Const 0),
         Imov 1 (Const 0),
         IloadW 2 (Reg 1),
         Iadd 0 0 (Reg 2),
         Iadd 1 1 (Const 8),
         Ijmp (Const 2)]
        

--test5 ls = Seq.lookup 0 (see (run5 ls) (4* (Prelude.length ls))) == (sum ls)
test5 :: TestTree
test5 = testProperty "Test adding a list of inputs" $ \xs ->
   claimEqual (exec False prog5 (4 * (toEnum $ length xs)) xs)  (sum xs)




--progAshr :: Program Reg MWord
progAshr
  :: Num regT =>
     MWord -> MWord -> [Instruction' regT regT (Operand regT MWord)]
progAshr input shift = 
  [ Imov 5 o1',
    Ishr  nsign 5 (Const $ toEnum $ (finiteBitSize zerow -1)),
    Imull sign  (nsign) (Const $ monew),
    Ixor  ret'' (sign) o1' ,
    Ishr  ret'  (ret'') o2',
    Ixor  ret   (ret') (Reg sign)]
  where o1' = Const input
        o2' = Const shift
        nsign = 4
        sign  = 3
        ret'' = 2
        ret'  = 1
        ret   = 0
        zerow,monew :: MWord
        zerow = 0
        monew = 0-1 
  
--test6 ls = Seq.lookup 0 (see (run5 ls) (4* (Prelude.length ls))) == (sum ls)
testAshr :: TestTree
testAshr = testProperty "Test implementation of arithmetic shift right"
        $ \inpt -> (inpt :: Int) == inpt ==>
                   \shift ->  (shift :: Int) >= 0 ==>
                              exec False (progAshr (binaryFromInt inpt) (binaryFromInt shift)) 6 [] ==
                              (binaryFromInt $ inpt `shiftR` shift)
binaryFromInt :: Int -> MWord
binaryFromInt n =
  if n >= 0 then toEnum n else maxBound - (toEnum (-1- n))
_binaryToInt :: MWord -> Int
_binaryToInt w =
  let halfBound :: MWord; halfBound = maxBound `quot` 2 in  
  if w <= halfBound then fromEnum w else - (fromEnum ((maxBound - w))) - 1


-- | Tests for `AbsMemory` loads and stores.
testAbsMem :: TestTree
testAbsMem =
  testGroup "Test implementation of abstract memory" [
    mkTest "8-byte store" W1 (map VExact [8, 7, 6, 5, 4, 3, 2, 1, 0, 0]) emptyMem $
      Abs.amStore W8 0x1000 (VExact 0x0102030405060708),
    mkTest "amStore case 1" W1 (map VExact [2, 2]) emptyMem $
      Abs.amStore W2 0x1000 (VExact 0x0202) .
      Abs.amStore W2 0x1000 (VExact 0x0101),
    mkTest "amStore case 2a" W1 (map VExact [2, 2, 1, 1]) emptyMem $
      Abs.amStore W2 0x1000 (VExact 0x0202) .
      Abs.amStore W4 0x1000 (VExact 0x01010101),
    mkTest "amStore case 2b" W1 (map VExact [1, 1, 2, 2, 0]) emptyMem $
      Abs.amStore W2 0x1002 (VExact 0x0202) .
      Abs.amStore W4 0x1000 (VExact 0x01010101),
    mkTest "amStore case 3" W1 (map VExact [1, 1, 1, 1]) emptyMem $
      Abs.amStore W4 0x1000 (VExact 0x01010101) .
      Abs.amStore W2 0x1000 (VExact 0x0202) .
      Abs.amStore W1 0x1003 (VExact 0x03),
    mkTest "amLoad case 1" W2 (map VExact [0x0102]) emptyMem $
      Abs.amStore W2 0x1000 (VExact 0x0102),
    -- The first load is case 2a, the second is 2b.
    mkTest "amLoad case 2a/2b" W2 (map VExact [0x0304, 0x0102]) emptyMem $
      Abs.amStore W4 0x1000 (VExact 0x01020304),
    mkTest "amLoad case 3" W2 (map VExact [0]) emptyMem id,
    mkTest "amLoad case 4" W4 (map VExact [0x02000101]) emptyMem $
      Abs.amStore W2 0x1000 (VExact 0x0101) .
      Abs.amStore W1 0x1003 (VExact 0x02),
    -- Regression test for a bug in amLoad case 2b / amStore case 2b
    mkTest "Load just after store" W1 [VExact 0, VExact 0, VTop] Abs.amHavoc $
      Abs.amStore W2 0x1000 (VExact 0)
  ]
  where
    mkTest name loadWidth loadExpect initMem updateMem = testCase name $ do
      let mem = initMem & Abs.amMem %~ updateMem
      forM_ (zip [0..] loadExpect) $ \(i, expected) -> do
        let actual = Abs.amLoad loadWidth
              (0x1000 + i * fromIntegral (widthInt loadWidth)) mem
        actual @?= expected

    emptyMem = Abs.amInit mempty
