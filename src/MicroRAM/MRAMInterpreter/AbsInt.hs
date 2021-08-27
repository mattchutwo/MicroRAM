{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MicroRAM.MRAMInterpreter.AbsInt where

import Control.Lens (makeLenses, (^.), (&), (.~), (%~), use, (.=))
import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.IRs
import Compiler.Metadata (Metadata(..))
import Compiler.Registers
import MicroRAM
import MicroRAM.MRAMInterpreter.Generic
import Util.Util


data AbsValue =
  -- | A set containing only a single value.
    VExact MWord
  -- | The set of all values.
  | VTop
  deriving (Show, Eq)

data AbsMemory = AbsMemory {
  -- | Gives the width and value of the most recent store at each address.
  -- Entries are always well-aligned, with the `MWord` address being a multiple
  -- of the `MemWidth`.  Furthermore, entries never overlap: a `W8` write to
  -- `0x1000` will overwrite any entries in the range `0x1000 .. 0x1008`.
  _amMem :: Map MWord (MemWidth, AbsValue),
  -- | Default value for addresses not present in `amMem`.  Should be either
  -- `VExact 0` or `VTop`.
  _amDefault :: AbsValue,
  _amPoisonedZero :: Bool
}
  deriving (Show)

makeLenses ''AbsMemory

type AbsMemoryMap = Map MWord (MemWidth, AbsValue)

amInit :: AbsMemoryMap -> AbsMemory
amInit m = AbsMemory m (VExact 0) False

amHavoc :: AbsMemory
amHavoc = AbsMemory mempty VTop True

-- | Load a value from abstract memory.  `addr` is assumed to be aligned to a
-- multiple of `widthInt w`.
amLoad :: MemWidth -> MWord -> AbsMemory -> AbsValue
amLoad w addr am
  -- Case 1: the load's address and width exactly matches the store.
  | Just (w', val) <- mid, w' == w = val
  -- Case 2a: the address matches but the width is larger.  We are loading a
  -- prefix (low bits) of the value that was stored.
  | Just (w', val) <- mid, w' > w = val `absAnd` VExact mask
  -- Case 2b: a store at a lower address covers this load.  We are loading some
  -- bits out of the middle of the value that was stored.  Since both the load
  -- and the store are well aligned, and the store is wider, the load must
  -- access a strict subset of the memory affected by the store.
  | Nothing <- mid, Just (addr', (w', val)) <- Map.lookupMax before,
    w' > w, addr' + fromIntegral (widthInt w') > addr =
    (val `absShr` VExact (fromIntegral $ 8 * (addr - addr'))) `absAnd` VExact mask
  -- Case 3: no stores cover any part of the load.
  | null parts = am ^. amDefault
  -- Case 4: several smaller stores cover the load.
  | otherwise = combine (am ^. amDefault) 0 parts' `absAnd` VExact mask
  where
    (before, mid, after) = Map.splitLookup addr $ am ^. amMem

    mask = (1 `shiftL` (8 * widthInt w)) - 1

    -- In case 4, we gather up all of the pieces that overlap `addr..addr + w`,
    -- and combine them all together.  Note that since every memory op must be
    -- aligned, we are guaranteed that all the pieces have width strictly less
    -- than `w` and lie entirely within the load region.

    endAddr = addr + fromIntegral (widthInt w)
    parts :: [(MWord, (MemWidth, AbsValue))]
    parts = maybe [] (\x -> [(addr, x)]) mid ++
      takeWhile (\(k, _) -> k < endAddr) (Map.toList after)

    -- | Add padding bytes to fill any gaps between the previous stores.
    go _pos [] = []
    go pos ((a, (w, v)) : rest) =
      replicate (fromIntegral $ a - pos) (W1, am ^. amDefault) ++ [(w, v)] ++
        go (a + fromIntegral (widthInt w)) rest

    parts' = go addr parts

    combine acc _off [] = acc
    combine acc off ((w, v) : rest) =
      let acc' = acc `absOr` (v `absShl` VExact off)
          off' = off + fromIntegral (8 * widthInt w)
      in combine acc' off' rest

-- | Store a value into abstract memory.  `addr` is assumed to be aligned to a
-- multiple of `widthInt w`.
amStore :: MemWidth -> MWord -> AbsValue -> AbsMemoryMap -> AbsMemoryMap
amStore w addr val mem
  -- Case 1: the store exactly overwrites a previous value of the same width.
  | Just (w', _) <- mid, w' == w = Map.insert addr (w, val) mem
  -- Case 2a: the store overwrites a prefix of a wider value.
  | Just (w', val') <- mid, w' > w =
    Map.insert addr (w, val) $ Map.union mem $ removePartMap addr w' val' addr w
  -- Case 2b: the store overwrites a middle portion of a wider value.
  | Nothing <- mid, Just (addr', (w', val')) <- Map.lookupMax before,
    w' > w, addr' + fromIntegral (widthInt w') > addr =
    Map.insert addr (w, val) $ Map.union mem $ removePartMap addr' w' val' addr w
  -- Case 3: the store overwrites zero or more smaller values.
  | otherwise = Map.insert addr (w, val) $ Map.withoutKeys mem overlappedKeys
  where
    (before, mid, after) = Map.splitLookup addr mem

    removePart ::
      MWord -> MemWidth -> AbsValue -> MWord -> MemWidth -> [(MWord, MemWidth, AbsValue)]
    removePart addr w val addr' w'
      -- `addr' .. end'` doesn't overlap `addr .. end`.
      | addr >= end' || addr' >= end = [(addr, w, val)]
      -- The regions overlap and have the same length.  Since all addresses
      -- must be aligned, this means the cutout region overlaps the entire
      -- value.
      --
      -- The `w' > w` case should never happen, but if it does, it means the
      -- entire value is covered by the cutout.
      | w' >= w = []
      -- The regions overlap but the cutout only covers part of it.  Due to the
      -- alignment requirement, the cutout must be entirely in one half or the
      -- other.
      | otherwise =
        removePart addr prevW lo addr' w' ++
        removePart (addr + fromIntegral (widthInt prevW)) prevW hi addr' w'
      where
        end = addr + fromIntegral (widthInt w)
        end' = addr' + fromIntegral (widthInt w')

        prevW = pred w
        mask = (1 `shiftL` (8 * widthInt prevW)) - 1
        lo = val `absAnd` VExact mask
        hi = (val `absShr` VExact (fromIntegral $ 8 * widthInt prevW)) `absAnd` VExact mask

    removePartMap ::
      MWord -> MemWidth -> AbsValue -> MWord -> MemWidth -> AbsMemoryMap
    removePartMap addr width val addr' width' =
      Map.fromList [(a, (w, v)) | (a, w, v) <- removePart addr width val addr' width']

    overlappedKeys = Set.fromList $
      takeWhile (\a -> a < addr + fromIntegral (widthInt w)) $ Map.keys after


instance AbsDomain AbsValue where
  type Memory AbsValue = AbsMemory

  absInitMem mem = amInit mem'
    where
      mem' = Map.mapKeysMonotonic (* fromIntegral wordBytes) $
        fmap (\x -> (WWord, VExact x)) flatMem
      flatMem = flatInitMem mem

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
    _ -> absMerge t e
    

  absStore w addr val mem = case addr of
    VExact addr' -> do
      checkAlign w addr'
      return $ mem & amMem %~ amStore w addr' val
    VTop -> do
      return $ mem
        & amMem .~ mempty
        & amDefault .~ VTop

  absLoad w addr mem = case addr of
    VExact addr' -> do
      checkAlign w addr'
      let val = amLoad w addr' mem
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


-- | Produce a value representing the union of `x` and `y`.
absMerge :: AbsValue -> AbsValue -> AbsValue
absMerge (VExact x) (VExact y) | x == y = VExact x
absMerge _ _ = VTop





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

checkAlign :: MemWidth -> MWord -> Hopefully ()
checkAlign w addr = do
  when (addr `mod` fromIntegral (widthInt w) /= 0) $ do
    otherError $ "unaligned access of " ++ show (widthInt w) ++ " bytes at " ++ showHex addr

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


data ExtraState = ExtraState {
  _eMetadata :: Seq Metadata
}
makeLenses ''ExtraState

type AbsIntState r = InterpState' r AbsValue ExtraState

data BranchCond =
  -- | The branch is always taken.
    BcAlways
  -- | The branch is taken if this `AbsValue` is nonzero.
  | BcNonZero AbsValue
  -- | The branch is taken if this `AbsValue` is zero.
  | BcZero AbsValue
  deriving (Show)

data StopReason =
  -- | Stopped at a jump to a concrete destination.  If the instruction that
  -- caused the stop is a conditional branch, then this result indicates that
  -- the branch was taken (the condition holds).
    SrJump MWord
  -- | Stopped at a conditional jump whose condition doesn't hold, so execution
  -- falls through to the next instruction.
  | SrFallThrough
  -- | Stopped at a symbolic branch.  If the branch condition holds,
  -- then the PC is updated to the given value.
  | SrBranch BranchCond AbsValue
  -- | Stopped at an `Ianswer` instruction that terminates the program with the
  -- given value.
  | SrAnswer AbsValue
  -- | Stopped at an instruction that invalidates the trace.
  | SrInvalid
  -- | Stopped because the caller requested a stop at this PC.
  | SrRequested
  deriving (Show)

data Stop r = Stop {
  -- | The instruction that execution stopped on.
  _stInstr :: Instruction r MWord,
  -- | The metadata of the instruction.
  _stMetadata :: Metadata,
  -- | The program state prior to executing the instruction.
  _stState :: AbsIntState r,
  -- | The reason why execution stopped.
  _stReason :: StopReason
}
makeLenses ''Stop

-- | Apply the effect of a branch instruction to state `s`.  Useful for
-- creating the next state when execution stops with reason `SrJump` or
-- `SrBranch`.
takeBranch :: MWord -> AbsIntState r -> AbsIntState r
takeBranch dest s = s
  & sMach . mPc .~ dest
  & sMach . mCycle %~ absAdd (VExact 1)

takeFallThrough :: AbsIntState r -> AbsIntState r
takeFallThrough s = s
  & sMach . mPc %~ (+ 1)
  & sMach . mCycle %~ absAdd (VExact 1)

maybeStepInstr :: (Regs r) =>
  Instruction r MWord -> InterpM' r AbsValue s Hopefully (Maybe StopReason)
maybeStepInstr i = case i of

  Ijmp op2 -> do
    dest <- opVal op2
    case dest of
      VExact dest' -> return $ Just $ SrJump dest'
      _ -> return $ Just $ SrBranch BcAlways dest

  Icjmp r2 op2 -> do
    flag <- regVal r2
    dest <- opVal op2
    case (flag, dest) of
      (VExact flag', VExact dest')
        | flag' /= 0 -> return $ Just $ SrJump dest'
        | otherwise -> return $ Just SrFallThrough
      _ -> return $ Just $ SrBranch (BcNonZero flag) dest

  Icnjmp r2 op2 -> do
    flag <- regVal r2
    dest <- opVal op2
    case (flag, dest) of
      (VExact flag', VExact dest')
        | flag' == 0 -> return $ Just $ SrJump dest'
        | otherwise -> return $ Just SrFallThrough
      _ -> return $ Just $ SrBranch (BcZero flag) dest

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
      sMach . mReg rd .= VTop
      finishInstr
      return Nothing
    XMalloc _ -> do
      sMach . mReg rd .= VTop
      finishInstr
      return Nothing

  _ -> stepInstr i >> return Nothing

fetchMetadata :: MWord -> StateT (AbsIntState r) Hopefully Metadata
fetchMetadata pc = do
  metas <- use $ sExt . eMetadata
  case Seq.lookup (fromIntegral pc) metas of
    Just m -> return m
    Nothing -> assumptError $ "missing metadata for instruction: " ++ show pc

-- | Run a single basic block.
runBlock :: (Regs r) => AbsIntState r -> Hopefully (Stop r)
runBlock s = evalStateT go s
  where
    go = do
      pc <- use $ sMach . mPc
      regs <- use $ sMach . mRegs
      i <- fetchInstr pc
      optReason <- maybeStepInstr i
      case optReason of
        Just reason -> do
          meta <- fetchMetadata pc
          s' <- get
          return $ Stop i meta s' reason
        Nothing -> go

-- | Run a section of straight-line code.  Continues through any concrete
-- control flow (`SrJump` / `SrFallThrough`) except for function calls and
-- returns.
runStraightLine :: (Regs r) => AbsIntState r -> Hopefully (Stop r, [MWord])
runStraightLine s = runStraightLine' s Nothing

mkRequestedStop :: StateT (AbsIntState r) Hopefully (Stop r)
mkRequestedStop = do
  pc <- use $ sMach . mPc
  i <- fetchInstr pc
  meta <- fetchMetadata pc
  s' <- get
  return $ Stop i meta s' SrRequested

runStraightLine' :: (Regs r) => AbsIntState r -> Maybe MWord -> Hopefully (Stop r, [MWord])
runStraightLine' s optStopPc = go [] s
  where
    go blks s
      | Just stopPc <- optStopPc, s ^. sMach . mPc == stopPc = do
        stop <- evalStateT mkRequestedStop s
        return (stop, reverse blks)
      | otherwise = do
        let pc = s ^. sMach . mPc
        let blks' = pc : blks
        stop <- runBlock s
        let concreteJumpDest = case stop ^. stReason of
              SrJump dest -> Just dest
              SrFallThrough -> Just $ (stop ^. stState . sMach . mPc) + 1
              _ -> Nothing
            md = stop ^. stMetadata
            isCallOrReturn = mdIsCall md || mdIsReturn md
        case concreteJumpDest of
          Just dest | not isCallOrReturn -> go blks' (takeBranch dest $ stop ^. stState)
          _ -> return (stop, reverse blks')

{-
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
-}

initState :: Regs r => ProgAndMem (AnnotatedProgram Metadata r MWord) -> AbsIntState r
initState (ProgAndMem prog mem _) =
  InterpState {
    _sMach = MachineState {
      _mCycle = VExact 0,
      _mPc = 0,
      _mRegs = initBank (VExact $ lengthInitMem mem) (VExact 0),
      _mProg = Seq.fromList $ map fst prog,
      _mMem = amem,
      _mBug = False,
      _mAnswer = Nothing
    },
    _sExt = ExtraState {
      _eMetadata = Seq.fromList $ map snd prog
    }
  }
  where
    (pubMem, secMem) = flatInitMem' mem
    pubMemMap = fmap (\x -> (WWord, VExact x)) pubMem
    secMemMap = fmap (\_ -> (WWord, VTop)) secMem
    memMap = Map.mapKeysMonotonic (* fromIntegral wordBytes) $ Map.union pubMemMap secMemMap
    amem = amInit memMap

havocState :: Regs r => AbsIntState r -> AbsIntState r
havocState s =
  InterpState {
    _sMach = MachineState {
      _mCycle = VTop,
      _mPc = s ^. sMach . mPc,
      _mRegs = initBank VTop VTop,
      _mProg = s ^. sMach . mProg,
      _mMem = amHavoc,
      _mBug = False,
      _mAnswer = Nothing
    },
    _sExt = ExtraState {
      _eMetadata = s ^. sExt . eMetadata
    }
  }
