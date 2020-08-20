{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module MicroRAM.InterpreterSpec where


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck


import Compiler.Registers
import Compiler.CompilationUnit

import MicroRAM.MicroRAM
import MicroRAM.MRAMInterpreter
import qualified Data.Sequence as Seq
import qualified Data.Map as Map


main :: IO ()
main = defaultMain tests
  -- defaultMain (testGroup "Our Library Tests" testSuite) -- testSuit defined at eof
k = 5 -- 5 registers

trivialCU :: Prog Int -> Word -> [Word] -> CompilationUnit (Prog Int)
trivialCU prog len input = CompUnit prog len InfinityRegs [] (list2InitMem input)

runProg :: Prog Int -> Word -> [Word] -> Trace Int
runProg prog len input = run $ trivialCU prog len input

-- We are treating the first register as the return
-- To get ouptu to get output provide a program and a number of steps to get the result after that long
-- Execute gets the trace and then looks at the first register after t steps
exec prog steps input = lookupReg sp (seeRegs (runProg prog (steps+1) input) (fromEnum steps)) -- this throws an error if there is no register 0
simpl_exec prog steps = exec prog steps [] -- when there are no inputs
seeRegs:: Trace mreg -> Int -> RMap mreg Word
seeRegs t n = regs (t !! n)

-- The tester setup
type Reg = Int
instance Regs Int where
  sp = 0
  bp = 1
  ax = 2
  -- argc = 3
  -- argv = 4
  fromWord = fromIntegral . toInteger
  toWord = fromIntegral
  data RMap Int x = RMap x (Map.Map Int x)
  initBank d = RMap d Map.empty
  lookupReg r (RMap d m) = case Map.lookup r m of
                        Just x -> x
                        Nothing -> d
  updateBank r x (RMap d m) = RMap d (Map.insert r x m)

get_regs :: State mreg -> RMap mreg Word
get_regs = regs

reg_trace::Trace mreg -> [RMap mreg Word]
reg_trace t= map regs t

pc_trace::Trace Reg -> [Word]
pc_trace t= map pc t

flag_trace::Trace Reg -> [Bool]
flag_trace t= map flag t



-- # Test 1
{- (1+2)*(3+4) = 21
-}

prog1 :: Program Reg Word
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

prog2 :: Program Reg Word
prog2 = [Iadd 0 0 (Const 1),
       Ijmp (Const 0)]

test2 = testProperty "Test `x++` on a loop" $ \n -> (n :: Word) >= 0 ==> simpl_exec prog2 (2*n) == fromIntegral n

-- # Test 3: fibonacci
{-
   x = 1
   while true {
     z = x
     x = x + y
     y = z
   } 
-}

fib_pure :: Word -> Word
fib_pure 0 = 0
fib_pure 1 = 1
fib_pure n = fib_pure (n-1) + fib_pure (n-2) 

prog3 :: Program Reg Word
prog3 = [Iadd 0 1 (Const 1), -- x=1
         Iadd 2 0 (Const 0), -- z=x
         Iadd 0 0 (Reg 1),  -- x=x+y
         Iadd 1 2 (Const 0),
       Ijmp (Const 1)]

fibs:: [Word]
fibs = 0 : 1 : Prelude.zipWith (+) fibs (tail fibs)
fib::Int -> Word
fib n = fibs !! n

claimEqual :: (Eq a, Show a) => a -> a -> Either String String
claimEqual a b = 
   if a == b
     then Right "OK"
     else Left $ "Got " ++ show a ++ " but expected " ++ show b

test3 = testProperty "Test fibonacci" $ \n -> (n :: Word) >= 0 ==>
   claimEqual (simpl_exec prog3 (1+4*n)) (fib_pure (n+1))
   
-- # Test 4: conditional + input

prog4 :: Program Reg Word
prog4 = [Iread 1 (Const 0), --
         Icmpg 1 (Const 10), -- 1
         Icjmp (Const 5),    -- 2 
         Iadd 0 0 (Const 77),-- 3
         Ijmp (Const 6),     -- 4
         Iadd 0 0 (Const 42), -- Label: 5
         Ijmp (Const 6) -- Label: 6
        ]

test4 = testProperty "Test a conditional and input" $ \x ->
   claimEqual (exec prog4 5 [x]) (if (x::Word)>10 then 42 else 77)

                                                               
-- # Test 5: sum all input
  {- for i in input
        x =+ i

NOTE: the initial memory contains, the size of the initial memory.
      so we start at mem=2            
-}


prog5 :: Program Reg Word
{- Old verison with tapes:
prog5 = [Iread 1 (Const 0), Iadd 0 0 (Reg 1), Icjmp (Const 4), Ijmp (Const 0), Ijmp (Const 4)]
-}
-- New version with input in initial memory.
prog5 = [Imov 1 (Const 0),
         Iload 2 (Reg 1),
         Iadd 0 0 (Reg 2),
         Iadd 1 1 (Const 1),
         Ijmp (Const 1)]
        
list2InitMem :: [Word] -> InitialMem
list2InitMem ls = map word2InitSeg $ zip [0..] ls 
  where word2InitSeg :: (Word,Word) -> InitMemSegment
        word2InitSeg (loc,val) = InitMemSegment False False loc 1 (Just [val]) 

--test5 ls = Seq.lookup 0 (see (run5 ls) (4* (Prelude.length ls))) == (Just $ sum ls)
test5 = testProperty "Test adding a list of inputs" $ \xs ->
   claimEqual (exec prog5 (4 * (toEnum $ length xs)) xs)  (sum xs)


                                                               
-- # Test 6: Arithmetics and overflows
  {- We test a bunch of arithmetic operations
     and the respective overflow. If anything
     goes wrong we jump to a fail state that
     sets r0 = 42 and loops
-}


failSignal:: Operand Reg Word
failSignal = (Const 11)
gotoFail = Ijmp failSignal
cGotoFail = Icjmp failSignal

prog6 :: Program Reg Word
prog6 = [Imov 1 (Const 1),   -- 0. x = 1 // Test 1
         Isub 1 1 (Const 2), -- 1. x = x - 2 (should underflow)
         Icjmp  (Const 4),   -- 2. goto Test 2
         gotoFail,           -- 3. fail if f = 0
         Imov 2 (Const 10),  -- 4. y = 10 // Test 2
         Iadd 2 2 (Const 20),-- 5. y += 20 (should not overflow and set flag to 0
         cGotoFail,          -- 6. fail if f=1
         Iadd 2 2 (Reg 1),  -- 7. y = x + y (Should overflow) // Test 3
         Icjmp  (Const 10),  -- 8. goto Test 2
         gotoFail,           -- 9 fail if f = 0
         Ijmp (Const 10),    -- 0. loop forever
         Imov 0 (Const 42),  -- 1. signals Failure 
         gotoFail      -- 2. loops 
        ]

--test6 ls = Seq.lookup 0 (see (run5 ls) (4* (Prelude.length ls))) == (Just $ sum ls)
test6 = testProperty "Test over/underflow for adition and substraction"
        $ \n -> (n :: Word) >= 0 ==> simpl_exec prog6 n /= 42

tests = testGroup "Testing the Interpreter for  MicroRAM" [test1,test2,test3,test4,test5, test6]
