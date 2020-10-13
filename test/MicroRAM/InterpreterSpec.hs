{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module MicroRAM.InterpreterSpec (main) where


import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck.Property as Prop (succeeded, failed, reason, Result)

import Compiler.Registers
import Compiler.CompilationUnit

import Data.Bits

import MicroRAM
import MicroRAM.MRAMInterpreter




main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing the Interpreter for  MicroRAM"
  [test1,test2,test3,test4,test5, testAshr]

-- * Pretty printers
_ppList :: Show a => [a] -> IO ()
_ppList all =  putStr $ concat $ map (\st -> show st ++ "\n") all

-- * Build a Compilation unit from a simpl program snip

-- | Build initial memory from a list of words

-- Old strategy
list2InitMem :: [MWord] -> InitialMem
list2InitMem ls = map word2InitSeg $ zip [0..] ls 
  where word2InitSeg :: (MWord,MWord) -> InitMemSegment
        word2InitSeg (loc,val) = InitMemSegment False False loc 1 (Just [val]) 



trivialCU :: Prog Int -> Word -> [MWord] -> CompilationResult (Prog Int)
trivialCU prog len input = CompUnit progs len InfinityRegs [] (list2InitMem input) ()
  where progs = MultiProg prog prog

runProg :: Prog Int -> Word -> [MWord] -> Trace Int
runProg prog len input = run $ trivialCU prog len input

-- We are treating the first register as the return
-- To get ouptu to get output provide a program and a number of steps to get the result after that long
-- Execute gets the trace and then looks at the first register after t steps
exec :: Prog Int -> Word -> [MWord] -> Maybe MWord
exec prog steps input = lookupReg sp (seeRegs (runProg prog (steps+1) input) (fromEnum steps)) -- this throws an error if there is no register 0
simpl_exec :: Prog Int -> Word -> Maybe MWord
simpl_exec prog steps = exec prog steps [] -- when there are no inputs
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
test1 = testProperty ("Testing (1+2)*(3+4) == 21.")  $ (simpl_exec prog1 5) == Just 21



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
                                                    simpl_exec prog2 (2*n) == (Just $ fromIntegral n)

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
                                              (simpl_exec prog3 (1+4*n)) ==  (Just $ fib_pure (n+1))
   
-- # Test 4: conditional + input

prog4 :: Program Reg MWord
prog4 = [Iload 1 (Const 0), --
         Icmpa 1 (Const 10), -- 1
         Icjmp (Const 5),    -- 2 
         Iadd 2 2 (Const 77),-- 3
         Ijmp (Const 6),     -- 4
         Iadd 2 2 (Const 42) -- Label: 5
        ,Imov 0 (Reg 2)      -- Label: 6
        , Ijmp (Const 6)     -- Label: 7
        ]

test4 :: TestTree
test4 = testProperty "Test a conditional and input" $ \x ->
   claimEqual (exec prog4 6 [x]) (Just $ if x>10 then 42 else 77)

                                                               
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
         Iload 2 (Reg 1),
         Iadd 0 0 (Reg 2),
         Iadd 1 1 (Const 1),
         Ijmp (Const 2)]
        

--test5 ls = Seq.lookup 0 (see (run5 ls) (4* (Prelude.length ls))) == (Just $ sum ls)
test5 :: TestTree
test5 = testProperty "Test adding a list of inputs" $ \xs ->
   claimEqual (exec prog5 (4 * (toEnum $ length xs)) xs)  (Just $ sum xs)




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
  
--test6 ls = Seq.lookup 0 (see (run5 ls) (4* (Prelude.length ls))) == (Just $ sum ls)
testAshr :: TestTree
testAshr = testProperty "Test implementation of arithmetic shift right"
        $ \inpt -> (inpt :: Int) == inpt ==>
                   \shift ->  (shift :: Int) >= 0 ==>
                              exec (progAshr (binaryFromInt inpt) (binaryFromInt shift)) 6 [] ==
                              Just (binaryFromInt $ inpt `shiftR` shift)
binaryFromInt :: Int -> MWord
binaryFromInt n =
  if n >= 0 then toEnum n else maxBound - (toEnum (-1- n))
_binaryToInt :: MWord -> Int
_binaryToInt w =
  let halfBound :: MWord; halfBound = maxBound `quot` 2 in  
  if w <= halfBound then fromEnum w else - (fromEnum ((maxBound - w))) - 1
