{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MicroRAM.MRAMInterpreter.Generic
  ( AbsDomain(..),
    MachineState'(..), mCycle, mPc, mRegs, mProg, mMem, mBug, mAnswer, 
    mReg,
    InterpState'(..), sExt, sMach,
    InterpM', InstrHandler',
    doStore, doLoad, doPoison, doGetPoison, doGetValue,
    fetchInstr, stepInstr, nextPc, finishInstr, regVal, opVal,
    stepStoreValue, stepLoadValue, stepPoisonValue
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Lens (makeLenses, lens, (.=), (%=), use)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
-- import Data.Map.Strict (Map)

import Compiler.Errors
import Compiler.CompilationUnit (InitialMem)
import Compiler.Registers
import MicroRAM


class (Show v, Show (Memory v)) => AbsDomain v where
  type Memory v

  absInitMem :: InitialMem -> Memory v

  absExact :: MWord -> v

  -- Arithemtic operations
  absAdd :: v -> v -> v
  absSub :: v -> v -> v
  absUMul :: v -> v -> (v, v)
  absSMul :: v -> v -> (v, v)
  absDiv :: v -> v -> v
  absMod :: v -> v -> v
  absNeg :: v -> v

  -- Bitwise operations
  absAnd :: v -> v -> v
  absOr :: v -> v -> v
  absXor :: v -> v -> v
  absNot :: v -> v
  absShl :: v -> v -> v
  absShr :: v -> v -> v

  -- Comparisons.  These return either zero or one.
  absEq :: v -> v -> v
  absUGt :: v -> v -> v
  absUGe :: v -> v -> v
  absSGt :: v -> v -> v
  absSGe :: v -> v -> v

  -- | `absMux c t e`: produces `t` if `c` is nonzero and `e` otherwise.
  absMux :: v -> v -> v -> v

  -- | The memory operations are assumed to be idempotent.
  -- E.g. storing the same value in the same locatoin twice is equivalent
  -- to just storing it once. 
  absStore :: MemWidth -> v -> v -> Memory v -> Hopefully (Memory v)
  absLoad :: MemWidth -> v -> Memory v -> Hopefully v
  absPoison :: MemWidth -> v -> Memory v -> Hopefully (Memory v)

  absGetPoison :: MemWidth -> v -> Memory v -> Hopefully Bool
  absGetValue :: v -> Hopefully MWord


data MachineState' r v = MachineState
  { _mCycle :: v
  , _mPc :: MWord
  , _mRegs :: RegBank r v
  , _mProg :: Seq (Instruction r MWord)
  , _mMem :: Memory v
  , _mBug :: Bool
  , _mAnswer :: Maybe MWord
  }  
makeLenses ''MachineState'

deriving instance (Show r, AbsDomain v) => Show (MachineState' r v)

data InterpState' r v s = InterpState
  { _sExt :: s
  , _sMach :: MachineState' r v
  }
makeLenses ''InterpState'

type InterpM' r v s m a = StateT (InterpState' r v s) m a
type InstrHandler' r v s = Instruction r MWord -> InterpM' r v s Hopefully ()


-- | Lens for accessing a particular register in `mRegs`.  Returns the default
-- value 0 when the register is uninitialized.
mReg :: (Functor f, Regs r, AbsDomain v) =>
  r -> (v -> f v) -> (MachineState' r v -> f (MachineState' r v))
mReg r = mRegs . lens (lookupReg r) (flip $ updateBank r)


-- | Lift a partial into the interpreter monad, annotating any errors that
-- occur with the current PC.
liftWrap :: Hopefully a -> InterpM' r v s Hopefully a
liftWrap f = do
  pc <- use $ sMach . mPc
  lift $ tag ("pc = " ++ show pc) f

doStore :: (AbsDomain v) => MemWidth -> v -> v -> InterpM' r v s Hopefully ()
doStore w addr val = do
  old <- use $ sMach . mMem
  new <- liftWrap $ absStore w addr val old
  sMach . mMem .= new
  
doLoad :: (AbsDomain v) => MemWidth -> v -> InterpM' r v s Hopefully v
doLoad w addr = do
  mem <- use $ sMach . mMem
  liftWrap $ absLoad w addr mem

doPoison :: (AbsDomain v) => MemWidth -> v -> InterpM' r v s Hopefully ()
doPoison w addr = do
  old <- use $ sMach . mMem
  new <- liftWrap $ absPoison w addr old
  sMach . mMem .= new

doGetPoison :: (AbsDomain v) => MemWidth -> v -> InterpM' r v s Hopefully Bool
doGetPoison w addr = do
  mem <- use $ sMach . mMem
  liftWrap $ absGetPoison w addr mem

doGetValue :: (AbsDomain v) => v -> InterpM' r v s Hopefully MWord
doGetValue x = liftWrap $ absGetValue x


-- | Fetch the instruction at `addr`.  Typically, `addr` will be the current
-- program counter (`sMach . mPc`).
fetchInstr :: MWord -> InterpM' r v s Hopefully (Instruction r MWord)
fetchInstr addr = do
  prog <- use $ sMach . mProg
  case Seq.lookup (fromIntegral addr) prog of
    Just x -> return x
    Nothing -> assumptError $ "program executed out of bounds: " ++ show addr

stepInstr :: (Regs r, AbsDomain v) => Instruction r MWord -> InterpM' r v s Hopefully ()
stepInstr i = do
  case i of
    Iand rd r1 op2 -> stepBinary absAnd rd r1 op2
    Ior rd r1 op2 -> stepBinary absOr rd r1 op2
    Ixor rd r1 op2 -> stepBinary absXor rd r1 op2
    Inot rd op2 -> stepUnary absNot rd op2

    Iadd rd r1 op2 -> stepBinary absAdd rd r1 op2
    Isub rd r1 op2 -> stepBinary absSub rd r1 op2
    Imull rd r1 op2 -> stepBinary (\a b -> fst $ absUMul a b) rd r1 op2
    Iumulh rd r1 op2 -> stepBinary (\a b -> snd $ absUMul a b) rd r1 op2
    Ismulh rd r1 op2 -> stepBinary (\a b -> snd $ absSMul a b) rd r1 op2
    -- TODO div/mod vs quot/rem?
    Iudiv rd r1 op2 -> stepBinary absDiv rd r1 op2
    Iumod rd r1 op2 -> stepBinary absMod rd r1 op2

    Ishl rd r1 op2 -> stepBinary absShl rd r1 op2
    Ishr rd r1 op2 -> stepBinary absShr rd r1 op2

    Icmpe r1 op1 op2 -> stepBinary absEq r1 op1 op2
    Icmpa r1 op1 op2 -> stepBinary absUGt r1 op1 op2
    Icmpae r1 op1 op2 -> stepBinary absUGe r1 op1 op2
    Icmpg r1 op1 op2 -> stepBinary absSGt r1 op1 op2
    Icmpge r1 op1 op2 -> stepBinary absSGe r1 op1 op2

    Imov rd op2 -> stepMove rd (Const 1) op2
    Icmov rd op1 op2 -> stepMove rd (Reg op1) op2

    Ijmp op2 -> stepJump (Const 1) True op2
    Icjmp r2 op2 -> stepJump (Reg r2) True op2
    Icnjmp r2 op2 -> stepJump (Reg r2) False op2

    Istore w op2 r1 -> stepStore w op2 r1
    Iload w rd op2 -> stepLoad w rd op2
    Ipoison w op2 r1 -> stepPoison w op2 r1

    Iread rd op2 -> stepRead rd op2
    Ianswer op2  -> stepAnswer op2

    Iadvise _ -> assumptError $ "unhandled advice request"

    i -> do
      -- Replace input operands with their values, and convert the destination
      -- register to a `Word` so it can be printed.
      i' <- mapInstrM (return . toWord) regVal opVal i
      assumptError $ "unhandled instruction: " ++ show i'

  sMach . mCycle %= (`absAdd` absExact 1)


stepUnary :: (Regs r, AbsDomain v) =>
  (v -> v) -> r -> Operand r MWord -> InterpM' r v s Hopefully ()
stepUnary f rd op2 = do
  y <- opVal op2
  let result = f y
  sMach . mReg rd .= result
  nextPc

stepBinary :: (Regs r, AbsDomain v) =>
  (v -> v -> v) -> r -> r -> Operand r MWord -> InterpM' r v s Hopefully ()
stepBinary f rd r1 op2 = do
  x <- regVal r1
  y <- opVal op2
  let result = f x y
  sMach . mReg rd .= result
  nextPc 

stepMove :: (Regs r, AbsDomain v) =>
  r -> Operand r MWord -> Operand r MWord -> InterpM' r v s Hopefully ()
stepMove rd cond op2 = do
  y <- opVal op2
  c <- opVal cond
  old <- use $ sMach . mReg rd
  sMach . mReg rd .= absMux c y old
  nextPc

stepJump :: (Regs r, AbsDomain v) =>
  Operand r MWord -> Bool -> Operand r MWord -> InterpM' r v s Hopefully ()
stepJump cond pos op2 = do
  y <- opVal op2 >>= doGetValue
  cond' <- opVal cond >>= doGetValue
  if pos `xnor` (0 /= cond') then sMach . mPc .= y else nextPc
    where xnor = (==)

checkPoison :: AbsDomain v => MemWidth -> v -> InterpM' r v s Hopefully ()
checkPoison w addr = do
  poisoned <- doGetPoison w addr
  when poisoned $ sMach . mBug .= True

stepStoreValue :: (Regs r, AbsDomain v) =>
  MemWidth -> Operand r MWord -> r -> InterpM' r v s Hopefully (v, v)
stepStoreValue w op2 r1 = do
  addr <- opVal op2
  val <- regVal r1
  checkPoison w addr
  doStore w addr val
  nextPc
  -- For advice, load the entire word at that location
  -- alternatively we could have `doStore` also return
  -- the fullWord stored. 
  fullStoredVal <- doLoad WWord addr 
  return (addr, fullStoredVal)

stepStore :: (Regs r, AbsDomain v) =>
  MemWidth -> Operand r MWord -> r -> InterpM' r v s Hopefully ()
stepStore w op2 r1 = do
  _ <- stepStoreValue w op2 r1
  return ()

stepLoadValue :: (Regs r, AbsDomain v) =>
  MemWidth -> r -> Operand r MWord -> InterpM' r v s Hopefully (v, v)
stepLoadValue w rd op2 = do
  addr <- opVal op2
  checkPoison w addr
  val <- doLoad w addr
  sMach . mReg rd .= val
  nextPc
  return (addr,val)
  
stepLoad :: (Regs r, AbsDomain v) =>
  MemWidth -> r -> Operand r MWord -> InterpM' r v s Hopefully ()
stepLoad w rd op2 = do
  _ <- stepLoadValue w rd op2
  return ()
  
stepPoisonValue :: (Regs r, AbsDomain v) =>
  MemWidth -> Operand r MWord -> r -> InterpM' r v s Hopefully (v, v)
stepPoisonValue w op2 r1 = do
  addr <- opVal op2
  val <- regVal r1
  doStore w addr val
  doPoison w addr
  nextPc
  return (addr, val)

stepPoison :: (Regs r, AbsDomain v) =>
  MemWidth -> Operand r MWord -> r -> InterpM' r v s Hopefully ()
stepPoison w op2 r1 = do
  _ <- stepPoisonValue w op2 r1
  return ()

stepRead :: (Regs r, AbsDomain v) =>
  r -> Operand r MWord -> InterpM' r v s Hopefully ()
stepRead rd _op2 = do
  -- All tapes are empty.
  sMach . mReg rd .= absExact 0
  nextPc

stepAnswer :: (Regs r, AbsDomain v) =>
  Operand r MWord -> InterpM' r v s Hopefully ()
stepAnswer op2 = do
  y <- opVal op2 >>= doGetValue
  sMach . mAnswer .= Just y
  -- No `nextPc` here.  We just keep looping on the `answer` instruction until
  -- the interpreter stops running.

nextPc :: AbsDomain v => InterpM' r v s Hopefully ()
nextPc = sMach . mPc %= (+ 1)

finishInstr :: AbsDomain v => InterpM' r v s Hopefully ()
finishInstr = do
  sMach . mPc %= (+ 1)
  sMach . mCycle %= (`absAdd` absExact 1)


regVal :: (Regs r, AbsDomain v) => r -> InterpM' r v s Hopefully v
regVal r = use $ sMach . mReg r

opVal ::  (Regs r, AbsDomain v) => Operand r MWord -> InterpM' r v s Hopefully v
opVal (Reg r) = regVal r
opVal (Const w) = return $ absExact w
