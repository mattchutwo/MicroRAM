{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
module MicroRAM.MRAMInterpreter
  ( Mem,
    --Tape, 
    State(..),
    Prog,
    Trace,
    run,
    execAnswer,
    --init_state,
    load,
    initMem, emptyInitMem -- should this go elsewhere?
    ) where

import MicroRAM.MicroRAM
import Data.Bits
import Data.Word
import Control.Exception
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))

import Compiler.Registers

{-
Module      : MRAM Interpreter
Description : Interpreter for MicrRAM programs
Maintainer  : santiago@galois.com
Stability   : experimental

The interpreter runs a MicroRAM pprogram, producing a
Trace of states that can easily be inspected. Current
semantics follows the TinyRAM paper.

Notes:
* Current implementation uses (Words) but it follows an interface
  that can be easily converted to any other Num
* Operations are performed over unbounded Integer and converted
  back anf forth from Wrd
* Follows the semantics in TinyRAM:
  https://www.scipr-lab.org/doc/TinyRAM-spec-0.991.pdf
-}

-- * MicroRAM semantics

type Wrd = Word
wrdMax = toInteger (maxBound :: Word32)
wrdMin = toInteger (minBound :: Word32)

toInt :: Integral a => a -> Int
toInt x = fromIntegral x

-- Most significant bit depends on implementation
-- If it's int then msb is the positive/negative marker
msb :: Wrd -> Bool 
msb x = x > (maxBound `quot` 2)

-- Some binary operations that are representation dependent

-- | Multiply and take the most significant bits.
umulh :: Integer -> Integer -> Integer
umulh r1 r2 = (r1 * r2) `quot` (wrdMax +1) -- this quotient removes the first W bits

-- | Multiply SIGNED and take the most significant bits.
-- We convert the operands through Int to interpret them as signed.
-- Then Multiply, then take the most significant bits  
smulh :: Integer -> Integer -> Integer
smulh r1 r2 = (r1' * r2') `quot` (wrdMax + 1)
  where r1' = toInteger $ toInt r1 -- By converting through Int, int's interpreted as signed.
        r2' = toInteger $ toInt r2  -- By converting through Int, int's interpreted as signed.


-- ** Program State

-- The Program Counter is a special register, represented separatedly 
type Pc = Wrd
init_pc = 0

-- | Registers
-- We represent the registers a a list of words,

-- The condition and bad flags
{- Current implementation of the flag only works for conditionals
 it does not get set on binary operations (e.g. on overflow) as in tiniyRAM

 We add a bad flag to be raised when an operation goes wrong. If the flag is
 set, the the rest of the state is bogus.
-}

init_flag = False

-- | Memory
-- Memory used to be
-- > Mem::Wrd -> Wrd 
-- but that is not good to building a (finite) trace
-- also we want programs that read uninitialized memory to bad


type Mem = (Wrd,Map.Map Wrd Wrd)

-- | Initial memory is given as input to the program.
init_mem :: [Word] -> Mem
init_mem input = (0,Map.fromList $ zip [0..] extendedInput)
  where -- | Add the special locations 1 and 0 with null pointer and input size
    extendedInput = [0 , fromIntegral $ length input] ++ input

store ::  Wrd -> Wrd -> Mem -> Mem
store x y (d,m)=  (d,Map.insert x y m) 

load ::  Wrd -> Mem -> Wrd
load x (d,m)=  case Map.lookup x m of
                 Just y -> y
                 Nothing -> d


-- *** Tapes: OBSOLETE input is passed as initial memory.
--type Tape = [Wrd]  -- ^read only tape

-- ** Program state State
{- I don't include the program in the state since it never changes

-}


-- | The program state 
data State mreg = State {
  pc :: Pc
  , regs :: RMap mreg Word 
  , mem :: Mem
  --, tapes :: (Tape, Tape)
  , flag :: Bool
  , bad :: Bool
  , answer :: Word }

deriving instance (Read (RMap mreg Word)) => Read (State mreg)
deriving instance (Show (RMap mreg Word)) => Show (State mreg)

  
init_state :: Regs mreg => [Word] -> State mreg
init_state input  = State {
  pc = init_pc
  , regs = initBank 0
  , mem = init_mem input
  --, tapes = (t_input, t_advice)
  , flag = init_flag
  , bad = init_flag
  , answer = 0
}

set_reg:: Regs mreg => mreg -> Wrd -> State mreg -> State mreg
set_reg r x st = st { regs = updateBank r x (regs st) }

store_mem::  Wrd -> Wrd -> State mreg -> State mreg
store_mem r x st = st {
   mem = store r x (mem st)
}

set_flag:: Bool -> State mreg  -> State mreg
set_flag b st = st {
   flag = b
}

set_pc:: Wrd -> State mreg -> State mreg
set_pc pc' st = st {
  pc = pc'
}

-- Turn on the bad flag
-- there is no way to "unbad" a state
set_bad:: State mreg -> State mreg
set_bad st= st {
   bad = True
}
data Side = LeftSide | RightSide

get_pair::Side -> (a,a) -> a
get_pair LeftSide = fst
get_pair RightSide = snd

set_pair::Side -> a -> (a,a) -> (a,a)
set_pair LeftSide a (_, b)= (a, b)
set_pair RightSide b (a, _) = (a,b)

--get_tape::State mreg -> Side -> Tape
--get_tape st b = get_pair b (tapes st)

to_side:: Wrd -> Maybe Side
to_side 0 = Just LeftSide
to_side 1 = Just RightSide
to_side _ = Nothing

--pop::Tape -> Maybe (Wrd, Tape)
--pop (x:tp) = Just (x,tp)
--pop _ = Nothing

--set_tape::Side -> Tape -> State mreg -> State mreg
--set_tape sd tp st  =  st {
--  tapes = set_pair sd tp (tapes st)
--  , flag = False -- ^ changing the tape always sets the flag to 0 (as per Tiny RAM semantics)
--}

set_answer:: Word -> State mreg -> State mreg
set_answer ans st  =  st { answer = ans }

-- Pop tape tries to pop a value from tape tp_n and store it in register r
-- if the tape is empty (or tp_n > 2) set r = 0 and flag = 1
-- if success set flag = 0
--pop_tape:: Regs mreg => Wrd -> mreg -> State mreg -> State mreg
--pop_tape tp_n r st =
--  case try_pop_tape st tp_n r of
--    Just st' -> set_flag False st'
--    _ -> set_flag True (set_reg r 0 st)
--  where try_pop_tape st tp_n r = do
--          sd <- to_side tp_n
--          (x,tp) <- pop (get_tape st sd)
--          Just $ set_reg r x $ set_tape sd tp st

next:: State mreg -> State mreg
next st = set_pc (succ $ pc st) st

-- * Interpreter

-- ** Utility evaluators

-- | Register getters (from a set register set)
get_reg :: Regs mreg => RMap mreg Word -> mreg -> Wrd
get_reg rs r = lookupReg r rs

eval_reg st r = get_reg (regs st) r

-- | Gets operand wether it's a register or a constant or a PC
eval_operand :: Regs mreg => State mreg -> Operand mreg Wrd -> Wrd
eval_operand st (Reg r) = eval_reg st r
eval_operand st (Const w) = w

-- *** unary and binart operations
{- The way we computeto do binary/unary operations we do the following steps:
   1 - Compute the operands (results are type Wrd)
   2 - Transforms the operands to Integer
   3 - Compute the operation over the integers
   4 - Transform the result to Wrd and store it in the return register
   5 - Set the flag, if the given condition is satisfied over the result

We use Integers to be homogeneus over all possible types Wrd and because it makes checking under/overflow easier
-}

-- | Binary operations generic.
bop :: (Regs mreg) =>
       State mreg
       -> mreg
       -> Operand mreg Wrd
       -> (Integer -> Integer -> x) -- ^ Binary operation
       -> x
bop rs r1 a f = f (toInteger $ get_reg (regs rs) r1) (toInteger $ eval_operand rs a)

  
-- | Unart operations generic. 
uop :: Regs mreg =>
       State mreg
       -> Operand mreg Wrd
       -> (Integer -> x)
       -- -> (Integer -> Bool) -- ^ Set the flag? Is applied to the result of the operation 
       -> x
uop rs a f = f (toInteger $ eval_operand rs a)


-- | Catches division by 0
-- By TinyRAM semantics, this sets the flag to 0 and returns 0
-- I would like to flag this as an error.
exception :: Regs mreg => 
             Bool
          -> mreg
          -> (State mreg -> State mreg) -- ^ continuation
          -> State mreg
          -> State mreg
exception False _ f st = f st
exception True r _ st = set_flag True $ set_reg r 0 st
catchZero :: Regs mreg =>
             Wrd
           -> mreg
          -> (State mreg -> State mreg) -- ^ continuation
          -> State mreg
          -> State mreg
catchZero w = exception (w == 0)

-- | Arithmetic modulo 32 to simulate Int32
-- This is a "hot fix".
-- TODO: adapt this based on the type of the instruction (e.g. support Int64)
modFromInteger x = fromInteger x `mod` 2^32


exec_bop :: Regs mreg =>
            State mreg
         -> mreg
         -> mreg
         -> Operand mreg Wrd
         -> (Integer -> Integer -> Integer) -- ^ Binary operation
         -> (Integer -> Bool) -- ^ Checks if flag should be set
         -> State mreg 
exec_bop st r1 r2 a f check = next $ set_flag (check result) $ set_reg r1 (modFromInteger result) st
  where result = bop st r2 a f

-- | Evaluate binop, but first check a<>0 
execBopCatchZero ::
  Regs mreg =>
  State mreg
  -> mreg
  -> mreg
  -> Operand mreg Wrd
  -> (Integer -> Integer -> Integer) -- ^ Binary operation
  -> State mreg 
execBopCatchZero st r1 r2 a f =
  catchZero (eval_operand st a) r1 (\st -> exec_bop st r1 r2 a quot (\_->True)) st 


exec_uop :: Regs mreg =>
            State mreg -> mreg -> Operand mreg Wrd
         -> (Integer -> Integer) -- ^ Unary operatio
         -> (Integer -> Bool) -- ^ Checks if flag should be set
         -> State mreg
exec_uop st r1 a f check = next $ set_flag (check result) $ set_reg r1 (fromInteger result) st
  where result = uop st a f


-- Common checks for binary operations (to set the flag)
isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False
notZero x = not (isZero x)


overflow :: Integer -> Bool
overflow i = i > wrdMax

borrow :: Integer -> Bool
borrow i = i < 0

overUnderflow :: Integer -> Bool
overUnderflow i = i < wrdMin || i > wrdMax


trivialCheck :: Integer -> Bool
trivialCheck _ = True

-- compute most/less significant digit
lsb :: Wrd -> Bool
lsb x = 0 == x `mod` 2
                 
-- *** Conditionals Util

exec_cnd :: Regs mreg =>
  State mreg
  -> mreg
  -> Operand mreg Wrd
  -> (Integer -> Integer -> Bool)
  -> State mreg
exec_cnd st r1 a f = next $ set_flag result st
                        where result = bop st r1 a f

-- *** Jump util

exec_jmp st a = set_pc (eval_operand st a) st

-- ** Instruction execution (after instr. fetching)

exec :: Regs mreg => Instruction mreg Wrd -> State mreg -> State mreg
exec (Iand r1 r2 a) st = exec_bop st r1 r2 a (.&.) isZero
exec (Ior r1 r2 a) st = exec_bop st r1 r2 a (.|.) isZero
exec (Ixor r1 r2 a) st = exec_bop st r1 r2 a xor isZero
exec (Inot r1 a) st = exec_uop st r1 a complement isZero

exec (Iadd r1 r2 a) st = exec_bop st r1 r2 a (+) overflow
exec (Isub r1 r2 a) st = exec_bop st r1 r2 a (-) borrow
exec (Imull r1 r2 a) st = exec_bop st r1 r2 a (*) overflow

exec (Iumulh r1 r2 a) st = exec_bop st r1 r2 a umulh notZero -- flagged iff the return is not zero (indicates overflow)
exec (Ismulh r1 r2 a) st = exec_bop st r1 r2 a smulh notZero  -- flagged iff the return is not zero (indicates overflow)
exec (Iudiv r1 r2 a) st = execBopCatchZero st r1 r2 a quot
exec (Iumod r1 r2 a) st = execBopCatchZero st r1 r2 a rem

-- Shifts are a bit tricky since the flag depends on the operand not the result.
exec (Ishl r1 r2 a) st = set_flag (msb $ eval_reg st r1) $
  exec_bop st r1 r2 a (\a b -> shiftL a (fromInteger b)) trivialCheck
exec (Ishr r1 r2 a) st = set_flag (lsb $ eval_reg st r1) $
  exec_bop st r1 r2 a (\a b -> shiftR a (fromInteger b)) trivialCheck

-- Compare operations
exec (Icmpe r1 a) st = exec_cnd st r1 a (==)
exec (Icmpa r1 a) st = exec_cnd st r1 a (>)
exec (Icmpae r1 a) st = exec_cnd st r1 a (>=)
exec (Icmpg r1 a) st = exec_cnd st r1 a (>)
exec (Icmpge r1 a) st = exec_cnd st r1 a (>=)

-- Move operations
exec (Imov r a) st = next $ set_reg r (eval_operand st a) st
exec (Icmov r a) st = if flag st
  then exec (Imov r a) st
  else next st
 
-- Jump Operations
exec (Ijmp a) st = exec_jmp st a
exec (Icjmp a) st = if flag st then exec_jmp st a else next st
exec (Icnjmp a) st = if not $ flag st then exec_jmp st a else next st

--Memory operations
exec (Istore a r1) st = next $ store_mem (eval_operand st a) (get_reg (regs st) r1) st
exec (Iload r1 a) st = next $ set_reg r1 (load (eval_operand st a) $ (mem st)) st
exec (Iread r1 a) st = next $ st -- pop_tape (eval_operand st a) r1 st

-- Answer : set answer to ans and loop (pc not incremented)
exec (Ianswer a) st =
  let ans = (eval_operand st a) in
    set_answer ans st   -- Set answer to ans
  --set_reg sp ans st -- sets register 0 (backwards compat. FIXME! )

-- ** Program step
type Prog mreg = Program mreg Wrd

step :: Regs mreg => Prog mreg  -> State mreg -> State mreg
step prog st = exec (prog !! (toInt $ pc st)) st


-- ** Execution
type Trace mreg = [State mreg]
run :: Regs mreg => [Word] -> Prog mreg -> Trace mreg
run inp prog = iterate (step prog) $ init_state inp

-- ** Some facilities to run
-- Simple getters to explore the trace.
get_regs :: State mreg -> RMap mreg Word
get_regs = regs

see_regs:: Trace mreg -> Int -> RMap mreg Word
see_regs t n = regs (t !! n)

reg_trace::Trace mreg -> [RMap mreg Word]
reg_trace t = map regs t

pc_trace'::Trace mreg -> [Wrd]
pc_trace' t= map pc t

flag_trace'::Trace mreg -> [Bool]
flag_trace' t= map flag t

-- Convenient execution
{- As long as we haven't implemented a "return",
   The return value will be stroed in the first register.
-}
k = 16

run' n prog = Prelude.take n (run [] prog)
pc_trace n prog = map pc (run' n prog)
out_trace n prog = map (\s-> lookupReg 0 (regs s)) (run' n prog)
flag_trace n prog = map flag (run' n prog)

execute :: Regs mreg => Prog mreg -> Int -> Wrd
execute prog n = lookupReg sp (regs $ (run [] prog) !! n)

execute_pc prog n = lookupReg sp ((see_regs $ run [] prog) n)

exec_input :: Regs mreg => Prog mreg -> [Word] -> Int -> Wrd
exec_input prog inp n = lookupReg sp ((see_regs $ run inp prog) n)


execAnswer :: Regs mreg => Prog mreg -> Int -> [Word] -> Word
execAnswer prog bound input = answer $ (run input prog) !! bound






-- | Create the initial memory from a list of inputs
-- TODO This seems out of place, but I don't know where to put it
initMem :: [String] -> [Word]
initMem ls = map fromIntegral $
  let argsAsChars = args2chars ("Name":ls) in   -- we fake the "name" of the program.
    let argv_array = getStarts argsAsChars in
      let argsAsString = concat argsAsChars in
        argsAsString ++
        argv_array ++
        [length argv_array, 2 + length argsAsString]
        
  where args2chars ls = map (addNull . str2Ascii) ls 
        addNull ls = (ls ++ [0])
        str2Ascii ls = map char2Ascii ls
        char2Ascii ch = fromEnum ch
        getStarts = getStartsRec 2 []

        getStartsRec _ ret [] = ret
        getStartsRec n ret (x:ls) =
          getStartsRec (n+length x) (ret++[n]) ls

emptyInitMem :: [Word]
emptyInitMem = initMem []
          
