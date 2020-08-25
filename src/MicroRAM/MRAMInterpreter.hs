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
    ExecutionState(..),
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
import Control.Monad
import Control.Monad.State

import Util.Util()

import Compiler.Registers
import Compiler.CompilationUnit


-- * Utility

wrdMax, _wrdMin :: Integer
wrdMax = toInteger (maxBound :: MWord)
_wrdMin = toInteger (minBound :: MWord)

wrdSize :: Int
wrdSize = finiteBitSize (0 :: MWord)

wrdModulus :: Integer
wrdModulus = 1 `shift` wrdSize

toInt :: Integral a => a -> Int
toInt x = fromIntegral x

-- Most significant bit depends on implementation
-- If it's int then msb is the positive/negative marker
msb :: MWord -> Bool 
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
type Pc = MWord
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
-- > Mem::MWord -> MWord 
-- but that is not good to building a (finite) trace
-- also we want programs that read uninitialized memory to bad


type Mem = (MWord,Map.Map MWord MWord)

-- | Initial memory is given as input to the program.
init_mem :: InitialMem -> Mem
init_mem input = (0,flatInitMem input)

-- | Write to a location in memory
store ::
  MWord     -- ^ addres
  -> MWord  -- ^ value
  -> Mem
  -> Mem
store x y (d,m)=  (d,Map.insert x y m) 

-- | Read from a location in memory
load ::  MWord -> Mem -> MWord
load x (d,m)=  case Map.lookup x m of
                 Just y -> y
                 Nothing -> d

-- *** Tapes: OBSOLETE input is passed as initial memory.
--type Tape = [MWord]  -- ^read only tape

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
data ExecutionState mreg = ExecutionState {
  -- | Program counter
  pc :: Pc
  -- | Register bank
  , regs :: RegBank mreg MWord
  -- | Memory state
  , mem :: Mem
  -- | Nondeterministic advice for the last step
  , advice :: [Advice] -- Deleted at the start of each step.
  --, tapes :: (Tape, Tape)
  -- | The flag (a boolean register)
  , flag :: Bool
  -- | Marks for bugs and invalid traces
  , bug_flag, inv_flag :: Bool
  -- | Return value.
  , answer :: MWord }

deriving instance (Read (RegBank mreg MWord)) => Read (ExecutionState mreg)
deriving instance (Show (RegBank mreg MWord)) => Show (ExecutionState mreg)

type ExecSt mreg = State (ExecutionState mreg)

init_state :: Regs mreg => InitialMem -> ExecutionState mreg
init_state input  = ExecutionState {
  pc = init_pc
  , regs = initBank (lengthInitMem input)
  , mem = init_mem input
  , advice = []
  --, tapes = (t_input, t_advice)
  , flag = init_flag
  , bug_flag = init_flag -- ^ Witness of a bug 
  , inv_flag = init_flag -- ^ trace doesn't give enough guarantees. FIXME: What does this reveal?
  , answer = 0
}

-- | Generic setter for changing the state
changeSt :: (ExecutionState mreg  -> ExecutionState mreg) -> ExecSt mreg ()
changeSt setter = do{ st <- get; put $ setter st }

getFromSt :: (ExecutionState mreg  -> a) -> ExecSt mreg a
getFromSt getter = do{ st <- get; return $ getter st }

setRegBank:: Regs mreg => RegBank mreg MWord -> ExecSt mreg ()
setRegBank regBank = changeSt (\st -> st {regs = regBank})

set_reg:: Regs mreg => mreg -> MWord -> ExecSt mreg ()
set_reg r x = do
  st <- get
  setRegBank $ updateBank r x (regs st)

store_advc :: MWord -> MWord -> ExecSt mreg ()
store_advc addr v = changeSt (\st -> st {advice = [MemOp addr v MOStore]})

load_advc :: MWord -> MWord -> ExecSt mreg ()
load_advc addr v = changeSt (\st -> st {advice = [MemOp addr v MOLoad]})

store_mem::  MWord -> MWord -> ExecSt mreg ()
store_mem r x = do
  st <- get
  put $ st { mem = store r x (mem st)}
  store_advc r x
  
load_mem :: Regs mreg => mreg -> MWord -> ExecSt mreg ()
load_mem r1 op = do
  st <- get
  value <- return $ load op $ (mem st) 
  load_advc op value
  set_reg r1 value 

set_flag :: Bool -> ExecSt mreg ()
set_flag b = changeSt (\st -> st { flag = b})

set_pc:: MWord -> ExecSt mreg ()
set_pc pc'= changeSt (\st ->  st {pc = pc'})

-- Turn on the bad flag
-- there is no way to "unbad" a state
bug:: ExecSt mreg ()
bug = changeSt (\st -> st {bug_flag = True})

_invalid:: ExecSt mreg ()
_invalid = changeSt (\st -> st {inv_flag = True})

set_answer:: MWord -> ExecSt mreg ()
set_answer ans  =  changeSt (\st -> st { answer = ans })


-- | Next increases the program counter 
next :: ExecSt mreg ()
next = do 
  st <- get
  put $ st {pc = (succ $ pc st)}

-- * Interpreter

-- ** Utility evaluators

-- | Register getters: if the register has not been initiates is an error !
get_bank :: ExecSt mreg (RegBank mreg MWord)
get_bank = getFromSt regs

get_reg :: Regs mreg => mreg -> ExecSt mreg MWord
get_reg r = do
  rs <- get_bank
  case lookupReg r rs of
    Just w -> return w
    Nothing -> do {bug; return 0}
      
get_flag :: ExecSt mreg Bool
get_flag = getFromSt flag

get_pc :: ExecSt mreg Pc
get_pc = getFromSt pc
 
-- | Gets operand wether it's a register or a constant or a PC
eval_operand :: Regs mreg => Operand mreg MWord -> ExecSt mreg MWord
eval_operand (Reg r) = get_reg r
eval_operand (Const w) = return w

-- *** unary and binart operations
{- The way we computeto do binary/unary operations we do the following steps:
   1 - Compute the operands (results are type MWord)
   2 - Transforms the operands to Integer
   3 - Compute the operation over the integers
   4 - Transform the result to MWord and store it in the return register
   5 - Set the flag, if the given condition is satisfied over the result

We use Integers to be homogeneus over all possible types MWord and because it makes checking under/overflow easier
-}

-- | Binary operations generic.
bop :: (Regs mreg) =>
       mreg
       -> Operand mreg MWord
       -> (Integer -> Integer -> x) -- ^ Binary operation
       -> ExecSt mreg x
bop r1 a f = do
  r1' <- get_reg r1
  a' <- eval_operand a
  return $ f (toInteger r1') (toInteger a')

  
-- | Unart operations generic. 
uop :: Regs mreg =>
       Operand mreg MWord
       -> (Integer -> x)
       -> ExecSt mreg x
uop a f = do
  a' <- eval_operand a
  return $ f (toInteger a')


-- | Catches division by 0
-- By TinyRAM semantics, this sets the flag to 0 and returns 0
-- I would like to flag this as an error.
exception :: Regs mreg => 
             Bool
          -> mreg
          -> ExecSt mreg () -- ^ continuation
          -> ExecSt mreg ()
exception False _ k = k
exception True r _ = do
  set_flag True
  set_reg r 0
catchZero :: Regs mreg =>
             MWord
           -> mreg
          -> ExecSt mreg () -- ^ continuation
          -> ExecSt mreg ()
catchZero w = exception (w == 0)
exec_bop :: Regs mreg =>
            mreg
         -> mreg
         -> Operand mreg MWord
         -> (Integer -> Integer -> Integer) -- ^ Binary operation
         -> (Integer -> Bool) -- ^ Checks if flag should be set
         -> ExecSt mreg () 
exec_bop r1 r2 a f check = do
  result <- bop r2 a f
  set_flag (check result)
  set_reg r1 (fromInteger result)
  next
  
-- | Evaluate binop, but first check a<>0 
execBopCatchZero ::
  Regs mreg =>
  mreg
  -> mreg
  -> Operand mreg MWord
  -> (Integer -> Integer -> Integer) -- ^ Binary operation
  -> ExecSt mreg ()
execBopCatchZero r1 r2 a f = do
  a' <- eval_operand a
  catchZero a' r1 $ exec_bop r1 r2 a f (\_-> False)
  -- It's unclear if we want to set the flag at all. 
  -- TinyRAM is abiguous: "flag is set to 1 if and only if [A]u = 0."
  -- Here the semantics is "flag is set to 1 is [A]=0 and to 0 otherwise."

exec_uop :: Regs mreg =>
            mreg
         -> Operand mreg MWord
         -> (Integer -> Integer) -- ^ Unary operatio
         -> (Integer -> Bool) -- ^ Checks if flag should be set
         -> ExecSt mreg ()
exec_uop r1 a f check = do
  result <- uop a f
  set_flag (check result)
  set_reg r1 (fromInteger result)
  next

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
lsb :: MWord -> Bool
lsb x = 0 == x `mod` 2
                 
-- *** Conditionals Util

exec_cnd :: Regs mreg =>
  mreg
  -> Operand mreg MWord
  -> (Integer -> Integer -> Bool)
  -> ExecSt mreg ()
exec_cnd r1 a f = do
  result <- bop r1 a f
  set_flag result
  next

-- *** Jump util

exec_jmp
  :: Regs mreg => Operand mreg MWord -> ExecSt mreg ()
exec_jmp a = do
  a' <-  eval_operand a
  set_pc a'

-- ** Instruction execution (after instr. fetching)
ifFlagExec :: ExecSt mreg a -> ExecSt mreg a -> ExecSt mreg a
ifFlagExec kt kf = do
  fl <- get_flag
  if fl then kt else kf
  
exec :: Regs mreg => Instruction mreg MWord -> ExecSt mreg ()
exec (Iand r1 r2 a) = exec_bop r1 r2 a (.&.) isZero
exec (Ior r1 r2 a)  = exec_bop r1 r2 a (.|.) isZero
exec (Ixor r1 r2 a) = exec_bop r1 r2 a xor isZero
exec (Inot r1 a)  = exec_uop r1 a complement isZero

exec (Iadd r1 r2 a) = exec_bop r1 r2 a (+) overflow
exec (Isub r1 r2 a) = exec_bop r1 r2 a (-) borrow
exec (Imull r1 r2 a) = exec_bop r1 r2 a (*) overflow

exec (Iumulh r1 r2 a) = exec_bop r1 r2 a umulh notZero -- ^ flagged iff the return is not zero (indicates overflow)
exec (Ismulh r1 r2 a) = exec_bop r1 r2 a smulh notZero  -- ^ flagged iff the return is not zero (indicates overflow)
exec (Iudiv r1 r2 a) = execBopCatchZero r1 r2 a quot
exec (Iumod r1 r2 a) = execBopCatchZero r1 r2 a rem

-- Shifts are a bit tricky since the flag depends on the operand not the result.
exec (Ishl r1 r2 a) = do
  r1' <- get_reg r1
  set_flag (msb r1')
  exec_bop r1 r2 a (\a b -> shiftL a (fromInteger b)) trivialCheck
exec (Ishr r1 r2 a) = do
  r1' <- get_reg r1
  set_flag (lsb r1')
  exec_bop r1 r2 a (\a b -> shiftR a (fromInteger b)) trivialCheck

-- Compare operations
exec (Icmpe r1 a) = exec_cnd r1 a (==)
exec (Icmpa r1 a) = exec_cnd r1 a (>)
exec (Icmpae r1 a) = exec_cnd r1 a (>=)
exec (Icmpg r1 a) = exec_cnd r1 a (>)
exec (Icmpge r1 a) = exec_cnd r1 a (>=)

-- Move operations
exec (Imov r a) = do
  a' <- eval_operand a
  set_reg r a'
  next
 
exec (Icmov r a) = ifFlagExec (exec (Imov r a)) next
 
-- Jump Operations
exec (Ijmp a) = exec_jmp a
exec (Icjmp a) = ifFlagExec (exec_jmp a) next
exec (Icnjmp a) = ifFlagExec next $ exec_jmp a -- Reverse order for not

--Memory operations
exec (Istore a r1) = do
  a' <- eval_operand a
  r1' <- get_reg r1
  store_mem a' r1'
  next

exec (Iload r1 a) =  do
  a' <- eval_operand a
  load_mem r1 a'
  next

exec (Iread _r1 _a) = next -- ^ Obsolete, it's a no-op now. FIXME: remove?
-- pop_tape (eval_operand st a) r1 st

-- Answer : set answer to ans and loop (pc not incremented)
exec (Ianswer a) = do
  a' <- eval_operand a
  set_answer a'

-- | freshAdvice: clear advice before every step 
freshAdvice :: ExecSt mreg ()
freshAdvice = changeSt (\st -> st {advice = []})

-- ** Program step
type Prog mreg = Program mreg MWord

step :: Regs mreg => Prog mreg  -> ExecSt mreg (ExecutionState mreg)
step prog = do
  pc' <- get_pc
  freshAdvice
  exec (prog !! (toInt $ pc'))
  get
_step_ :: Regs mreg => Prog mreg  -> ExecSt mreg ()
_step_ p = do {_ <- step p; return ()}

-- ** Execution
type Trace mreg = [ExecutionState mreg]

-- | Produce the trace of a program
run :: Regs mreg => CompilationUnit (Prog mreg) -> Trace mreg
run (CompUnit prog trLen _ _ initMem) =
  initSt : (evalState (replicateM (fromEnum trLen) (step prog)) $ initSt)
  where initSt = init_state initMem
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
  data RegBank Int x = RegBank x (Map.Map Int x)
  initBank d = RegBank d Map.empty
  lookupReg r (RegBank d m) = case Map.lookup r m of
                        Just x -> x
                        Nothing -> d
  updateBank r x (RegBank d m) = RegBank d (Map.insert r x m)


-}
