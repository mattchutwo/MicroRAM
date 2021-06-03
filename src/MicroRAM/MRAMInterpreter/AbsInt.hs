{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MicroRAM.MRAMInterpreter.AbsInt where

import Control.Lens (makeLenses, at, (^.), (&), (.~), use, (.=))
import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.IRs
import Compiler.Registers
import MicroRAM
import MicroRAM.MRAMInterpreter.Generic
import Util.Util

import Debug.Trace


data AbsValue =
  -- | A set containing only a single value.
    VExact MWord
  -- | The set of all values.
  | VTop
  deriving (Show)

data AbsMemory = AbsMemory {
  _amMem :: Map MWord AbsValue,
  _amPoisonedZero :: Bool
}
  deriving (Show)

makeLenses ''AbsMemory


instance AbsDomain AbsValue where
  type Memory AbsValue = AbsMemory

  absInitMem mem = AbsMemory (fmap VExact mem) False

  absExact x = VExact x


  absAdd a b = case (a, b) of
    (VExact a', VExact b') -> VExact $ a' + b'
    _ -> VTop

  absSub a b = case (a, b) of
    (VExact a', VExact b') -> VExact $ a' - b'
    _ -> VTop

  absUMul a b = case (a, b) of
    (VExact a', VExact b') ->
      let (lo, hi) = uMul a' b' in
      (VExact lo, VExact hi)
    _ -> (VTop, VTop)

  absSMul a b = case (a, b) of
    (VExact a', VExact b') ->
      let (lo, hi) = sMul a' b' in
      (VExact lo, VExact hi)
    _ -> (VTop, VTop)

  absDiv a b = case (a, b) of
    (VExact _, VExact 0) -> VExact 0
    (VExact a', VExact b') -> VExact $ a' `div` b'
    _ -> VTop

  absMod a b = case (a, b) of
    (VExact _, VExact 0) -> VExact 0
    (VExact a', VExact b') -> VExact $ a' `mod` b'
    _ -> VTop

  absNeg a = case a of
    VExact a' -> VExact $ negate a'
    _ -> VTop


  absAnd a b = case (a, b) of
    (VExact a', VExact b') -> VExact $ a' .&. b'
    _ -> VTop

  absOr a b = case (a, b) of
    (VExact a', VExact b') -> VExact $ a' .|. b'
    _ -> VTop

  absXor a b = case (a, b) of
    (VExact a', VExact b') -> VExact $ a' `xor` b'
    _ -> VTop

  absNot a = case a of
    VExact a' -> VExact $ complement a'
    _ -> VTop

  absShl a b = case (a, b) of
    (VExact a', VExact b') -> VExact $ a' `shiftL` fromIntegral b'
    _ -> VTop

  absShr a b = case (a, b) of
    (VExact a', VExact b') -> VExact $ a' `shiftR` fromIntegral b'
    _ -> VTop


  absEq a b = case (a, b) of
    (VExact a', VExact b') -> VExact $ if a' == b' then 1 else 0
    _ -> VTop

  absUGt a b = case (a, b) of
    (VExact a', VExact b') -> VExact $ if a' > b' then 1 else 0
    _ -> VTop

  absUGe a b = case (a, b) of
    (VExact a', VExact b') -> VExact $ if a' >= b' then 1 else 0
    _ -> VTop

  absSGt a b = case (a, b) of
    (VExact a', VExact b') -> VExact $ if toMInt a' > toMInt b' then 1 else 0
    _ -> VTop

  absSGe a b = case (a, b) of
    (VExact a', VExact b') -> VExact $ if toMInt a' >= toMInt b' then 1 else 0
    _ -> VTop


  absMux c t e = case c of
    VExact c'
      | c' == 0 -> e
      | otherwise -> t
    _ -> VTop
    

  absStore w addr val mem = case addr of
    VExact addr' -> do
      (waddr, offset) <- splitAlignedAddr w addr'
      let old = maybe (VExact 0) id (mem ^. amMem . at waddr)
      let new = case (old, val) of
            (VExact old', VExact val') -> VExact $ old' & subBytes w offset .~ val'
            _ -> VTop
      return $ mem & amMem . at waddr .~ Just new
    VTop -> do
      return $ mem & amMem .~ mempty

  absLoad w addr mem = case addr of
    VExact addr' -> do
      (waddr, offset) <- splitAlignedAddr w addr'
      let wval = maybe (VExact 0) id (mem ^. amMem . at waddr)
      let val = case wval of
            VExact wval' -> VExact $ wval' ^. subBytes w offset
            _ -> VTop
      return val
    VTop -> do
      return VTop

  absPoison WWord (VExact 0) mem = return $ mem & amPoisonedZero .~ True
  absPoison _w _addr mem = return mem

  absGetPoison WWord (VExact 0) mem = return $ mem ^. amPoisonedZero
  absGetPoison _w _addr _mem = return False

  absGetValue v = case v of
    VExact x -> return x
    _ -> otherError $ "can't concretize abstract value " ++ show v






-- TODO: copied from MRAMInterpreter; move somewhere common

subBytes :: Functor f => MemWidth -> Int -> (MWord -> f MWord) -> MWord -> f MWord
subBytes w off f x = put <$> f x'
  where
    offBits = off * 8
    mask = (1 `shiftL` (widthInt w * 8)) - 1
    -- Extract `w` bytes from `x` at `off`
    x' = (x `shiftR` offBits) .&. mask
    -- Insert `w` bytes of `y'` into `x` at `off`
    put y' = (x .&. complement (mask `shiftL` offBits)) .|. ((y' .&. mask) `shiftL` offBits)

splitAddr :: MWord -> (MWord, Int)
splitAddr a = (waddr, offset)
  where
    waddr = a `shiftR` logWordBytes
    offset = fromIntegral $ a .&. ((1 `shiftL` logWordBytes) - 1)

splitAlignedAddr :: MemWidth -> MWord -> Hopefully (MWord, Int)
splitAlignedAddr w a = do
  let (waddr, offset) = splitAddr a
  when (offset `mod` widthInt w /= 0) $ do
    otherError $ "unaligned access of " ++ show (widthInt w) ++ " bytes at " ++ showHex a
  return (waddr, offset)

uMul :: MWord -> MWord -> (MWord, MWord)
uMul a b = (lo, hi)
  where
    prod = toInteger a * toInteger b
    lo = fromInteger prod
    hi = fromInteger $ prod `shiftR` wordBits

sMul :: MWord -> MWord -> (MWord, MWord)
sMul a b = (lo, hi)
  where
    prod = toSignedInteger a * toSignedInteger b
    lo = fromIntegral prod
    hi = fromIntegral $ prod `shiftR` wordBits

toMInt :: MWord -> MInt
toMInt = fromIntegral

toSignedInteger :: MWord -> Integer
toSignedInteger = toInteger . toMInt



data BranchCond =
  -- | The branch is always taken.
    BcAlways
  -- | The branch is taken if this `AbsValue` is nonzero.
  | BcNonZero AbsValue
  -- | The branch is taken if this `AbsValue` is zero.
  | BcZero AbsValue
  deriving (Show)

data StopReason =
  -- | `SrSymBranch cond dest`: the next instruction is a symbolic branch.  If
  -- `cond` holds, then control should transfer to `dest`; otherwise, it should
  -- fall through.
    SrSymBranch BranchCond AbsValue
  -- | `SrAnswer val`: the next instruction is an answer instruction, which
  -- terminates the program with code `val`.
  | SrAnswer AbsValue
  -- | `SrInvalid`: the next instruction invalidates the trace.
  | SrInvalid
  -- | `SrFuel`: not enough fuel to keep executing.
  | SrFuel
  deriving (Show)


maybeStepInstr :: (Regs r) =>
  Instruction r MWord -> InterpM' r AbsValue s Hopefully (Maybe StopReason)
maybeStepInstr i = case i of

  Ijmp op2 -> do
    dest <- opVal op2
    case dest of
      VExact _ -> stepInstr i >> return Nothing
      _ -> return $ Just $ SrSymBranch BcAlways dest

  Icjmp r2 op2 -> do
    flag <- regVal r2
    dest <- opVal op2
    case (flag, dest) of
      (VExact _, VExact _) -> stepInstr i >> return Nothing
      _ -> return $ Just $ SrSymBranch (BcNonZero flag) dest

  Icnjmp r2 op2 -> do
    flag <- regVal r2
    dest <- opVal op2
    case (flag, dest) of
      (VExact _, VExact _) -> stepInstr i >> return Nothing
      _ -> return $ Just $ SrSymBranch (BcZero flag) dest

  Ipoison _ op2 _ -> do
    addr <- opVal op2
    poisonedZero <- use $ sMach . mMem . amPoisonedZero
    case addr of
      VExact 0 | poisonedZero -> return $ Just SrInvalid
      _ -> stepInstr i >> return Nothing

  Ianswer op2 -> do
    val <- opVal op2
    return $ Just $ SrAnswer val

  Iextadvise rd ext -> case ext of
    XAdvisePoison _lo _hi -> do
      sMach . mReg rd .= VExact 0 -- TODO
      finishInstr
      return Nothing
    XMalloc _ -> do
      sMach . mReg rd .= VTop
      finishInstr
      return Nothing

  _ -> stepInstr i >> return Nothing

-- | Run a single path of execution.  Stops upon reaching a symbolic branch or
-- upon termination.
runPath :: (Regs r) => Int -> InterpM' r AbsValue s Hopefully StopReason
runPath 0 = return SrFuel
runPath n = do
  pc <- use $ sMach . mPc
  i <- fetchInstr pc
  optStop <- maybeStepInstr i
  case optStop of
    Just sr -> return sr
    Nothing -> runPath (n - 1)
  

initState :: Regs r => ProgAndMem (AnnotatedProgram m r MWord) -> s -> InterpState' r AbsValue s
initState (ProgAndMem prog mem) s =
  InterpState {
    _sExt = s,
    _sMach = MachineState {
      _mCycle = VExact 0,
      _mPc = 0,
      _mRegs = initBank (VExact $ lengthInitMem mem),
      _mProg = Seq.fromList $ map fst prog,
      _mMem = amem,
      _mBug = False,
      _mAnswer = Nothing
    }
  }
  where
    (pubMem, secMem) = flatInitMem' mem
    amem = AbsMemory (Map.union (fmap VExact pubMem) (fmap (const VTop) secMem)) False

testAbsInt_v :: Regs r => CompilationResult (AnnotatedProgram m r MWord) -> Hopefully ()
testAbsInt_v (CompUnit (MultiProg _ progMem) _ _ _ _ _) = do
  let st = initState progMem ()
  (sr, st') <- runStateT (runPath 10000) st
  traceM $ "stopped at " ++ show (st' ^. sMach . mPc) ++ " after " ++
    show (st' ^. sMach . mCycle) ++ " cycles"
  traceM $ "reason = " ++ show sr
  return ()
