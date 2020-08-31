{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-
Module      : MRAM Interpreter
Description : Interpreter for MicrRAM programs
Maintainer  : santiago@galois.com
Stability   : experimental

The interpreter runs a MicroRAM program, producing a
trace of states that can easily be inspected for correctness.
Current semantics follows the TinyRAM paper.
-}

module MicroRAM.MRAMInterpreter
  ( -- * Execute a program
    run, execAnswer,
    -- * Trace
    State(..),
    Trace, 
    Advice(..), renderAdvc,
    MemOpType(..),
    -- * Just for the debugger
    Mem, load,
    -- * Just for testing
    Prog
    ) where

import MicroRAM
import Data.Bits
import qualified Data.Map.Strict as Map
import GHC.Generics

import Util.Util

import Compiler.Registers
import Compiler.CompilationUnit


-- * MicroRAM semantics

type Wrd = MWord
wrdMax,_wrdMin :: Integer
wrdMax = toInteger (maxBound :: Wrd)
_wrdMin = toInteger (minBound :: Wrd)

wrdSize :: Int
wrdSize = finiteBitSize (0 :: Wrd)

wrdModulus :: Integer
wrdModulus = 1 `shift` wrdSize

toInt :: Integral a => a -> Int
toInt x = fromIntegral x

-- Most significant bit depends on implementation
-- If it's int then msb is the positive/negative marker
msb :: Wrd -> Bool 
msb x = testBit x (wrdSize - 1)

-- Some binary operations that are representation dependent

-- | Multiply and take the most significant bits.
umulh :: Integer -> Integer -> Integer
umulh r1 r2 = (r1 * r2) `quot` wrdModulus -- this quotient removes the first W bits

-- | Multiply SIGNED and take the most significant bits.
-- We convert the operands through Int to interpret them as signed.
-- Then Multiply, then take the most significant bits  
smulh :: Integer -> Integer -> Integer
smulh r1 r2 = (r1' * r2') `quot` wrdModulus
  where r1' = toInteger $ toInt r1 -- By converting through Int, int's interpreted as signed.
        r2' = toInteger $ toInt r2  -- By converting through Int, int's interpreted as signed.


-- ** Program State

-- The Program Counter is a special register, represented separatedly 
type Pc = Wrd
init_pc :: Pc
init_pc = 0

-- | Registers
-- We represent the registers a a list of words,

-- The condition and bad flags
{- Current implementation of the flag only works for conditionals
 it does not get set on binary operations (e.g. on overflow) as in tiniyRAM

 We add a bad flag to be raised when an operation goes wrong. If the flag is
 set, the the rest of the state is bogus.
-}
init_flag :: Bool
init_flag = False

-- | Memory
-- Memory used to be
-- > Mem::Wrd -> Wrd 
-- but that is not good to building a (finite) trace
-- also we want programs that read uninitialized memory to bad


type Mem =  (Wrd,Map.Map Wrd Wrd)

-- | Initial memory is given as input to the program.
init_mem :: InitialMem -> Mem
init_mem input = (0,flatInitMem input)

-- | Write to a location in memory
store ::
  Wrd     -- ^ addres
  -> Wrd  -- ^ value
  -> Mem
  -> Mem
store x y (d,m)=  (d,Map.insert x y m) 

-- | Read from a location in memory
load ::  Wrd -> Mem -> Wrd
load x (d,m)=  case Map.lookup x m of
                 Just y -> y
                 Nothing -> d


-- *** Tapes: OBSOLETE input is passed as initial memory.
--type Tape = [Wrd]  -- ^read only tape

-- ** Program state State
{- I don't include the program in the state since it never changes

-}
-- | Memory operation advice: nondeterministic advice to help the
-- witness checker chekc the memory consistency. 
data MemOpType = MOStore | MOLoad
  deriving (Eq, Read, Show, Generic)

-- | This information is passed the witness checker as nondeterministic
-- advice. Currently we only advice about memory operations and steps that stutter.
data Advice =
    MemOp
    MWord      -- ^ address
    MWord      -- ^ value
    MemOpType  -- ^ read or write
  | Stutter
  deriving (Eq, Read, Show, Generic)


-- | Pretty printer for advice.
renderAdvc :: [Advice] -> String
renderAdvc advs = concat $ map renderAdvc' advs
  where renderAdvc' :: Advice -> String
        renderAdvc' (MemOp addr v MOStore) = "Store: " ++ show addr ++ "->" ++ show v
        renderAdvc' (MemOp  addr v MOLoad) = "Load: " ++ show addr ++ "->" ++ show v
        renderAdvc' (Stutter) = "...Stutter..."

-- | The program state 
data State mreg = State {
  -- | Program counter
  pc :: Pc
  -- | Register bank
  , regs :: RMap mreg MWord
  -- | Memory state
  , mem :: Mem
  -- | Nondeterministic advice for the last step
  , advice :: [Advice] -- Deleted at the start of each step.
  --, tapes :: (Tape, Tape)
  -- | The flag (a boolean register)
  , flag :: Bool
  -- | Bad flag (another boolean register). Not used right now
  , bad :: Bool
  -- | Return value.
  , answer :: MWord }

deriving instance (Read (RMap mreg MWord)) => Read (State mreg)
deriving instance (Show (RMap mreg MWord)) => Show (State mreg)

  
init_state :: Regs mreg => InitialMem -> State mreg
init_state input  = State {
  pc = init_pc
  , regs = initBank 0 (lengthInitMem input)
  , mem = init_mem input
  , advice = []
  --, tapes = (t_input, t_advice)
  , flag = init_flag
  , bad = init_flag
  , answer = 0
}

set_reg:: Regs mreg => mreg -> Wrd -> State mreg -> State mreg
set_reg r x st = st { regs = updateBank r x (regs st) }

store_advc :: Wrd -> Wrd -> State mreg -> State mreg
store_advc addr v st = st {advice = [MemOp addr v MOStore] } 
load_advc :: Wrd -> Wrd -> State mreg -> State mreg
load_advc addr v st = st {advice = [MemOp addr v MOLoad] }


store_mem::  Wrd -> Wrd -> State mreg -> State mreg
store_mem r x st = store_advc r x $ 
  st { mem = store r x (mem st)}

load_mem :: Regs mreg => mreg -> Wrd -> State mreg -> State mreg
load_mem r1 op st =
  let value = (load op $ (mem st)) in
    set_reg r1 value $ load_advc op value st


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
_set_bad:: State mreg -> State mreg
_set_bad st= st {
   bad = True
}

set_answer:: MWord -> State mreg -> State mreg
set_answer ans st  =  st { answer = ans }

next :: State mreg -> State mreg
next st = set_pc (succ $ pc st) st

-- * Interpreter

-- ** Utility evaluators

-- | Register getters (from a set register set)
get_reg :: Regs mreg => RMap mreg MWord -> mreg -> Wrd
get_reg rs r = lookupReg r rs

eval_reg :: Regs mreg => State mreg -> mreg -> Wrd
eval_reg st r = get_reg (regs st) r

-- | Gets operand wether it's a register or a constant or a PC
eval_operand :: Regs mreg => State mreg -> Operand mreg Wrd -> Wrd
eval_operand st (Reg r) = eval_reg st r
eval_operand _st (Const w) = w

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


exec_bop :: Regs mreg =>
            State mreg
         -> mreg
         -> mreg
         -> Operand mreg Wrd
         -> (Integer -> Integer -> Integer) -- ^ Binary operation
         -> (Integer -> Bool) -- ^ Checks if flag should be set
         -> State mreg 
exec_bop st r1 r2 a f check = next $ set_flag (check result) $ set_reg r1 (fromInteger result) st
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
execBopCatchZero st r1 r2 a _f =
  catchZero (eval_operand st a) r1 (\st -> exec_bop st r1 r2 a quot (\_->True)) st 


exec_uop :: Regs mreg =>
            State mreg -> mreg -> Operand mreg Wrd
         -> (Integer -> Integer) -- ^ Unary operatio
         -> (Integer -> Bool) -- ^ Checks if flag should be set
         -> State mreg
exec_uop st r1 a f check = next $ set_flag (check result) $ set_reg r1 (fromInteger result) st
  where result = uop st a f


-- Common checks for binary operations (to set the flag)
isZero,notZero :: Integer -> Bool
isZero 0 = True
isZero _ = False
notZero x = not (isZero x)


overflow, borrow :: Integer -> Bool
overflow i = i > wrdMax
borrow i = i < 0

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

exec_jmp
  :: Regs mreg => State mreg -> Operand mreg Wrd -> State mreg
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
exec (Iload r1 a) st =  next $ load_mem  r1 (eval_operand st a) st

exec (Iread _r1 _a) st = next $ st -- pop_tape (eval_operand st a) r1 st

-- Answer : set answer to ans and loop (pc not incremented)
exec (Ianswer a) st =
  let ans = (eval_operand st a) in
    set_answer ans st   -- Set answer to ans
  --set_reg sp ans st -- sets register 0 (backwards compat. FIXME! )


-- | freshAdvice: clear advice before every step 
freshAdvice :: State mreg -> State mreg
freshAdvice st = st {advice = []}

-- ** Program step
type Prog mreg = Program mreg Wrd

step :: Regs mreg => Prog mreg  -> State mreg -> State mreg
step prog st = exec (prog !! (toInt $ pc st)) $ freshAdvice st


-- ** Execution
type Trace mreg = [State mreg]

-- | Produce the trace of a program
run :: Regs mreg => CompilationUnit (Prog mreg) -> Trace mreg
run (CompUnit prog trLen _ _ initMem) =
  takeEnum trLen $ 
  iterate (step prog) $ init_state initMem


-- ** Some facilities to run

-- | Execute the program and return the result.
execAnswer :: Regs mreg => CompilationUnit (Prog mreg) -> MWord
execAnswer compUnit = answer $ last $ run compUnit






-- | Create the initial memory from a list of inputs
-- TODO This seems out of place, but I don't know where to put it
_buildInitMem :: [String] -> [MWord]
_buildInitMem ls = map fromIntegral $
  let argsAsChars = args2chars ("Name":ls) in   -- we fake the "name" of the program.
    let argv_array = getStarts argsAsChars in
      let argsAsString = concat argsAsChars in
        argsAsString ++
        argv_array ++             -- argv
        [length argv_array,       -- arg C
         2 + length argsAsString] -- Points at itself (staring "stack pointer")
        
  where args2chars ls = map (addNull . str2Ascii) ls 
        addNull ls = (ls ++ [0])
        str2Ascii ls = map char2Ascii ls
        char2Ascii ch = fromEnum ch
        getStarts = getStartsRec 2 []

        getStartsRec _ ret [] = ret
        getStartsRec n ret (x:ls) =
          getStartsRec (n+length x) (ret++[n]) ls

_emptyInitMem :: [MWord]
_emptyInitMem = _buildInitMem []
          



-- Testing grounds

{-
prog1 :: Program Int MWord
prog1 = [Iadd 0 0 (Const 1),
       Iadd 0 0 (Const 2),
       Iadd 1 1 (Const 3),
       Iadd 1 1 (Const 4),
       Imull 0 0 (Reg 1)]
myCU len = CompUnit prog1 len InfinityRegs [] []

instance Regs Int where
  sp = 0
  bp = 1
  ax = 2
  argc = 3
  argv = 4
  fromWord = fromIntegral . toInteger
  toWord = fromIntegral
  data RMap Int x = RMap x (Map.Map Int x)
  initBank d = RMap d Map.empty
  lookupReg r (RMap d m) = case Map.lookup r m of
                        Just x -> x
                        Nothing -> d
  updateBank r x (RMap d m) = RMap d (Map.insert r x m)


-}
