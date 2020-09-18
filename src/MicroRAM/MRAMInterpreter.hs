{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
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
    Mem', load,
    -- * Just for testing
    Prog
    ) where

import Control.Monad
import Control.Monad.State
import Control.Lens (makeLenses, ix, at, lens, (.=), (%=), use, Lens')
import Data.Bits
import Data.List (intercalate)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as Text

import GHC.Generics

import Compiler.Errors
import Compiler.Registers
import Compiler.CompilationUnit
import MicroRAM

import Debug.Trace


data Mem = Mem MWord (Map MWord MWord)

data MachineState r = MachineState
  { _mCycle :: MWord
  , _mPc :: MWord
  , _mRegs :: RegBank r MWord
  , _mFlag :: Bool
  , _mProg :: Seq (Instruction r MWord)
  , _mMem :: Mem
  , _mAnswer :: Maybe MWord
  }
makeLenses ''MachineState

data InterpState r s = InterpState
  { _sExt :: s
  , _sMach :: MachineState r
  }
makeLenses ''InterpState

type InterpM r s m a = StateT (InterpState r s) m a

type InstrHandler r s = Instruction r MWord -> InterpM r s Hopefully ()


-- | Lens for accessing a particular register in `mRegs`.  Returns the default
-- value 0 when the register is uninitialized.
mReg :: (Functor f, Regs r) => r -> (MWord -> f MWord) -> (MachineState r -> f (MachineState r))
mReg r = mRegs . lens (maybe 0 id . lookupReg r) (flip $ updateBank r)

-- | Lens for accessing a particular word of memory.  Produces the `Mem`'s
-- default value when reading an uninitialized location.
memWord :: Functor f => MWord -> (MWord -> f MWord) -> (Mem -> f Mem)
memWord addr = lens (get addr) (set addr)
  where
    get addr (Mem d m) = maybe d id $ Map.lookup addr m
    set addr (Mem d m) val = Mem d $ Map.insert addr val m

mMemWord :: Functor f => MWord -> (MWord -> f MWord) -> (MachineState r -> f (MachineState r))
mMemWord addr = mMem . memWord addr


-- | Fetch the instruction at `addr`.  Typically, `addr` will be the current
-- program counter (`sMach . mPc`).
fetchInstr :: MWord -> InterpM r s Hopefully (Instruction r MWord)
fetchInstr addr = do
  prog <- use $ sMach . mProg
  case Seq.lookup (fromIntegral addr) prog of
    Just x -> return x
    Nothing -> assumptError $ "program executed out of bounds: " ++ show addr

stepInstr :: Regs r => Instruction r MWord -> InterpM r s Hopefully ()
stepInstr i = do
  case i of
    Iand rd r1 op2 -> stepBinary (.&.) (\_ _ r -> r == 0) rd r1 op2
    Ior rd r1 op2 -> stepBinary (.|.) (\_ _ r -> r == 0) rd r1 op2
    Ixor rd r1 op2 -> stepBinary xor (\_ _ r -> r == 0) rd r1 op2
    Inot rd op2 -> stepUnary complement (\_ r -> r == 0) rd op2

    Iadd rd r1 op2 -> stepBinary (+) addOverflow rd r1 op2
    Isub rd r1 op2 -> stepBinary (-) subUnderflow rd r1 op2
    Imull rd r1 op2 -> stepBinary (*) mulOverflow rd r1 op2
    Iumulh rd r1 op2 -> stepBinary umulh mulOverflow rd r1 op2
    Ismulh rd r1 op2 -> stepBinary smulh signedMulOverflow rd r1 op2
    -- TODO div/mod vs quot/rem?
    Iudiv rd r1 op2 -> stepBinary safeDiv (\_ y _ -> y == 0) rd r1 op2
    Iumod rd r1 op2 -> stepBinary safeMod (\_ y _ -> y == 0) rd r1 op2

    Ishl rd r1 op2 -> stepBinary shiftL' (\x _ _ -> msb x) rd r1 op2
    Ishr rd r1 op2 -> stepBinary shiftR' (\x _ _ -> lsb x) rd r1 op2

    Icmpe r1 op2 -> stepCompare (==) r1 op2
    Icmpa r1 op2 -> stepCompare (>) r1 op2
    Icmpae r1 op2 -> stepCompare (>=) r1 op2
    Icmpg r1 op2 -> stepCompare signedGt r1 op2
    Icmpge r1 op2 -> stepCompare signedGe r1 op2

    Imov rd op2 -> stepMove (const True) rd op2
    Icmov rd op2 -> stepMove (== True) rd op2

    Ijmp op2 -> stepJump (const True) op2
    Icjmp op2 -> stepJump (== True) op2
    Icnjmp op2 -> stepJump (== False) op2

    Istore op2 r1 -> stepStore op2 r1
    Iload rd op2 -> stepLoad rd op2

    Iread rd op2 -> stepRead rd op2
    Ianswer op2 -> stepAnswer op2

    Iext name _ -> assumptError $ "unhandled extension instruction " ++ show name
    Iextval name _ _ -> assumptError $ "unhandled extension instruction " ++ show name

  sMach . mCycle %= (+ 1)

stepUnary :: Regs r => (MWord -> MWord) -> (MWord -> MWord -> Bool) ->
  r -> Operand r MWord -> InterpM r s Hopefully ()
stepUnary f flag rd op2 = do
  y <- opVal op2
  let result = f y
  sMach . mReg rd .= result
  sMach . mFlag .= flag y result
  nextPc

stepBinary :: Regs r => (MWord -> MWord -> MWord) -> (MWord -> MWord -> MWord -> Bool) ->
  r -> r -> Operand r MWord -> InterpM r s Hopefully ()
stepBinary f flag rd r1 op2 = do
  x <- regVal r1
  y <- opVal op2
  let result = f x y
  sMach . mReg rd .= result
  sMach . mFlag .= flag x y result
  nextPc

stepCompare :: Regs r => (MWord -> MWord -> Bool) ->
  r -> Operand r MWord -> InterpM r s Hopefully ()
stepCompare flag r1 op2 = do
  x <- regVal r1
  y <- opVal op2
  sMach . mFlag .= flag x y
  nextPc

stepMove :: Regs r => (Bool -> Bool) -> r -> Operand r MWord -> InterpM r s Hopefully ()
stepMove cond rd op2 = do
  y <- opVal op2
  ok <- cond <$> use (sMach . mFlag)
  when ok $ sMach . mReg rd .= y
  nextPc

stepJump :: Regs r => (Bool -> Bool) -> Operand r MWord -> InterpM r s Hopefully ()
stepJump cond op2 = do
  y <- opVal op2
  ok <- cond <$> use (sMach . mFlag)
  if ok then sMach . mPc .= y else nextPc

stepStore :: Regs r => Operand r MWord -> r -> InterpM r s Hopefully ()
stepStore op2 r1 = do
  addr <- opVal op2
  val <- regVal r1
  sMach . mMemWord addr .= val
  nextPc

stepLoad :: Regs r => r -> Operand r MWord -> InterpM r s Hopefully ()
stepLoad rd op2 = do
  addr <- opVal op2
  val <- use $ sMach . mMemWord addr
  sMach . mReg rd .= val
  nextPc

stepRead :: Regs r => r -> Operand r MWord -> InterpM r s Hopefully ()
stepRead rd _op2 = do
  -- All tapes are empty.
  sMach . mReg rd .= 0
  sMach . mFlag .= True
  nextPc

stepAnswer :: Regs r => Operand r MWord -> InterpM r s Hopefully ()
stepAnswer op2 = do
  y <- opVal op2
  sMach . mAnswer .= Just y
  -- No `nextPc` here.  We just keep looping on the `answer` instruction until
  -- the interpreter stops running.

nextPc :: InterpM r s Hopefully ()
nextPc = sMach . mPc %= (+ 1)


wordBits :: Int
wordBits = finiteBitSize (0 :: MWord)

toSignedInteger :: MWord -> Integer
toSignedInteger x = toInteger x - if msb x then 1 `shiftL` wordBits else 0

addOverflow :: MWord -> MWord -> MWord -> Bool
addOverflow x _ r = r < x

subUnderflow :: MWord -> MWord -> MWord -> Bool
subUnderflow x _ r = r > x

mulOverflow :: MWord -> MWord -> MWord -> Bool
mulOverflow x y _ = toInteger x * toInteger y > toInteger (maxBound :: MWord)

signedMulOverflow :: MWord -> MWord -> MWord -> Bool
signedMulOverflow x y _ = r' < low || r' > high
  where
    r' = toInteger x * toInteger y
    low = negate $ 1 `shiftL` (wordBits - 1)
    high = (1 `shiftL` (wordBits - 1)) - 1

umulh :: MWord -> MWord -> MWord
umulh x y = fromInteger $ (toInteger x * toInteger y) `shiftR` wordBits

smulh :: MWord -> MWord -> MWord
smulh x y = fromInteger $ (toSignedInteger x * toSignedInteger y) `shiftR` wordBits

safeDiv :: MWord -> MWord -> MWord
safeDiv x y = if y == 0 then 0 else x `div` y

safeMod :: MWord -> MWord -> MWord
safeMod x y = if y == 0 then 0 else x `mod` y

shiftL' :: MWord -> MWord -> MWord
shiftL' x y = x `shiftL` fromIntegral y
shiftR' :: MWord -> MWord -> MWord
shiftR' x y = x `shiftR` fromIntegral y

lsb :: MWord -> Bool
lsb w = testBit w 0
msb :: MWord -> Bool
msb w = testBit w (wordBits - 1)

signedGt :: MWord -> MWord -> Bool
signedGt x y = toSignedInteger x > toSignedInteger y

signedGe :: MWord -> MWord -> Bool
signedGe x y = toSignedInteger x >= toSignedInteger y

regVal :: Regs r => r -> InterpM r s Hopefully MWord
regVal r = use $ sMach . mReg r

opVal ::  Regs r => Operand r MWord -> InterpM r s Hopefully MWord
opVal (Reg r) = regVal r
opVal (Const w) = return w


-- Advice-generating extension

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

type AdviceMap = Map MWord [Advice]

adviceHandler :: Regs r => Lens' s AdviceMap -> InstrHandler r s
adviceHandler advice (Istore op2 r1) = do
  addr <- opVal op2
  val <- regVal r1
  cycle <- use $ sMach . mCycle
  sExt . advice . at cycle .= Just [MemOp addr val MOStore]
adviceHandler advice (Iload _rd op2) = do
  addr <- opVal op2
  val <- use $ sMach . mMemWord addr
  cycle <- use $ sMach . mCycle
  sExt . advice . at cycle .= Just [MemOp addr val MOLoad]
adviceHandler _ _ = return ()


-- Trace handler (for debugging)

traceHandler :: Regs r => InstrHandler r s -> InstrHandler r s
traceHandler _nextH (Iext "trace" ops) = do
  vals <- mapM opVal ops
  traceM $ "TRACE " ++ intercalate ", " (map show vals)
  nextPc
traceHandler _nextH (Iext name ops) | Just desc <- Text.stripPrefix "trace_" name = do
  vals <- mapM opVal ops
  traceM $ "TRACE[" ++ Text.unpack desc ++ "] " ++ intercalate ", " (map show vals)
  nextPc
traceHandler nextH instr = nextH instr


-- Old public definitions

type Mem' = (MWord, Map MWord MWord)

-- | The program state 
data ExecutionState mreg = ExecutionState {
  -- | Program counter
  pc :: MWord
  -- | Register bank
  , regs :: RegBank mreg MWord
  -- | Memory state
  , mem :: Mem'
  -- | Nondeterministic advice for the last step
  , advice :: [Advice]
  -- | The flag (a boolean register)
  , flag :: Bool
  -- | Marks for bugs and invalid traces
  , bug_flag, inv_flag :: Bool
  -- | Return value.
  , answer :: MWord }

deriving instance (Read (RegBank mreg MWord)) => Read (ExecutionState mreg)
deriving instance (Show (RegBank mreg MWord)) => Show (ExecutionState mreg)

getStateWithAdvice :: Monad m => Lens' s AdviceMap -> InterpM r s m (ExecutionState r)
getStateWithAdvice advice = do
  pc <- use $ sMach . mPc
  regs <- use $ sMach . mRegs
  Mem d m <- use $ sMach . mMem
  let mem = (d, m)
  cycle <- use $ sMach . mCycle
  -- Retrieve advice for the cycle that just finished executing.
  adv <- use $ sExt . advice . ix (cycle - 1)
  flag <- use $ sMach . mFlag
  let bug = False
  let inv = False
  answer <- use $ sMach . mAnswer
  return $ ExecutionState pc regs mem adv flag bug inv (maybe 0 id answer)

type Prog mreg = Program mreg MWord
type Trace mreg = [ExecutionState mreg]

initMach :: Regs r => Program r MWord -> InitialMem -> MachineState r
initMach prog imem = MachineState
  { _mCycle = 0
  , _mPc = 0
  , _mRegs = initBank (lengthInitMem imem)
  , _mFlag = False
  , _mProg = Seq.fromList prog
  , _mMem = Mem 0 $ flatInitMem imem
  , _mAnswer = Nothing
  }

-- | Produce the trace of a program
run :: Regs mreg => CompilationResult (Prog mreg) -> Trace mreg
run (CompUnit prog trLen _ _ initMem _) = case runStateT (go trLen) initState of
  Left e -> error $ describeError e
  Right (x, _s) -> x
  where
    go 0 = return []
    go n = do
      -- The first of the `n` states is simply the initial state.  So we only
      -- run `n-1` steps of actual execution.
      s <- getStateWithAdvice id
      go' [s] (n - 1)

    go' tr 0 = return $ reverse tr
    go' tr n = do
      pc <- use $ sMach . mPc
      i <- fetchInstr pc
      adviceHandler id i
      (traceHandler $ stepInstr) i

      s <- getStateWithAdvice id
      go' (s : tr) (n - 1)

    initState = InterpState mempty $ initMach prog initMem

-- | Execute the program and return the result.
execAnswer :: Regs mreg => CompilationResult (Prog mreg) -> MWord
execAnswer compUnit = answer $ last $ run compUnit

-- | Read from a location in memory
load ::  MWord -> Mem' -> MWord
load x (d,m)=  case Map.lookup x m of
                 Just y -> y
                 Nothing -> d
