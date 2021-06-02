{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

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
    Executor,
    run, run_v, execAnswer, execBug,
    -- For post processing check (e.g. segment checking)
    runWith, initMach, InstrHandler, runPassGeneric, InterpState, sMach, sExt, mCycle,
    InstrHandler', InterpState',
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
import Control.Monad.Except
import Control.Monad.State
import Control.Lens (makeLenses, ix, at, to, lens, (^.), (^?), (&), (.~), (%~), (.=), (%=), use, Lens', _1, _2, _3)
import Data.Bits
import qualified Data.ByteString as BS
import Data.Foldable
import Data.List (intercalate)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word

import GHC.Generics (Generic)


import Compiler.Errors
import Compiler.Registers
import Compiler.CompilationUnit
import MicroRAM
import MicroRAM.MRAMInterpreter.Generic

import Util.Util

import Debug.Trace


-- AbsDomain instance for MWord (concrete interpreter)

data WordMemory = WordMemory {
    _wmDefault :: MWord,
    _wmMem :: Map MWord MWord,
    _wmPoison :: Set MWord
  }
  deriving (Show)
makeLenses ''WordMemory

toMInt :: MWord -> MInt
toMInt = fromIntegral

toSignedInteger :: MWord -> Integer
toSignedInteger = toInteger . toMInt

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

instance AbsDomain MWord where
  type Memory MWord = WordMemory

  absInitMem mem = WordMemory {
      _wmDefault = 0,
      _wmMem = mem,
      _wmPoison = mempty
    }

  absExact = id

  absAdd = (+)
  absSub = (-)
  absUMul a b = (lo, hi)
    where
      prod = toInteger a * toInteger b
      lo = fromInteger prod
      hi = fromInteger $ prod `shiftR` wordBits
  absSMul a b = (lo, hi)
    where
      prod = toSignedInteger a * toSignedInteger b
      lo = fromIntegral prod
      hi = fromIntegral $ prod `shiftR` wordBits
  absDiv a b = if b == 0 then 0 else a `div` b
  absMod a b = if b == 0 then 0 else a `mod` b
  absNeg = negate

  absAnd = (.&.)
  absOr = (.|.)
  absXor = xor
  absNot = complement
  absShl a b = a `shiftL` fromIntegral b
  absShr a b = a `shiftR` fromIntegral b

  absEq a b = if a == b then 1 else 0
  absUGt a b = if a > b then 1 else 0
  absUGe a b = if a >= b then 1 else 0
  absSGt a b = if toMInt a > toMInt b then 1 else 0
  absSGe a b = if toMInt a >= toMInt b then 1 else 0

  absMux c t e = if c /= 0 then t else e

  absStore w addr val mem = do
    (waddr, offset) <- splitAlignedAddr w addr
    let old = maybe (mem ^. wmDefault) id (mem ^. wmMem . at waddr)
    let new = old & subBytes w offset .~ val
    return $ mem & wmMem . at waddr .~ Just new

  absLoad w addr mem = do
    (waddr, offset) <- splitAlignedAddr w addr
    let wval = maybe (mem ^. wmDefault) id (mem ^. wmMem . at waddr)
    return $ wval ^. subBytes w offset

  absPoison w addr mem = do
    when (w /= WWord) $ otherError $ "bad poison width " ++ show w
    (waddr, _offset) <- splitAlignedAddr w addr
    return $ mem & wmPoison %~ Set.insert waddr

  absGetPoison w addr mem = do
    (waddr, _offset) <- splitAlignedAddr w addr
    return $ Set.member waddr (mem ^. wmPoison)

  absGetValue x = return x


type MachineState r = MachineState' r MWord
type InterpState r s = InterpState' r MWord s
type InterpM r s m a = InterpM' r MWord s m a
type InstrHandler r s = InstrHandler' r MWord s


-- | Lens for accessing a particular word of memory.  Produces the `Mem`'s
-- default value when reading an uninitialized location.
memWord :: Functor f => MWord -> (MWord -> f MWord) -> (WordMemory -> f WordMemory)
memWord addr = lens (get addr) (set addr)
  where
    get addr m = maybe (m ^. wmDefault) id $ m ^? wmMem . ix addr
    set addr m val = m & wmMem . at addr .~ Just val

mMemWord :: Functor f =>
  MWord ->
  (MWord -> f MWord) ->
  (MachineState' r MWord -> f (MachineState' r MWord))
mMemWord addr = mMem . memWord addr


-- Advice-generating extension

-- | Memory operation advice: nondeterministic advice to help the
-- witness checker chekc the memory consistency. 
data MemOpType = MOStore | MOLoad | MOPoison
  deriving (Eq, Read, Show, Generic)

-- | This information is passed the witness checker as nondeterministic
-- advice. Currently we only advice about memory operations and steps that stutter.
data Advice =
    MemOp
    MWord      -- ^ address
    MWord      -- ^ value
    MemOpType  -- ^ read or write
    MemWidth   -- ^ width of the access
  | Advise MWord
  | Stutter
  deriving (Eq, Read, Show, Generic)


-- | Pretty printer for advice.
renderAdvc :: [Advice] -> String
renderAdvc advs = concat $ map renderAdvc' advs
  where renderAdvc' :: Advice -> String
        renderAdvc' (MemOp addr v MOStore _w) = "Store: " ++ show addr ++ "->" ++ show v
        renderAdvc' (MemOp  addr v MOLoad _w) = "Load: " ++ show addr ++ "->" ++ show v
        renderAdvc' (MemOp addr v MOPoison _w) = "Poison: " ++ show addr ++ "->" ++ show v
        renderAdvc' (Advise v) = "Advise: " ++ show v
        renderAdvc' (Stutter) = "...Stutter..."

type AdviceMap = Map MWord [Advice]

-- | Helper function to record advice.  This is used by `adviceHandler` and
-- also by any other handlers that want to record advice of their own.
recordAdvice :: Regs r => Lens' s AdviceMap -> Advice -> InterpM r s Hopefully ()
recordAdvice adviceMap adv = do
  cycle <- use $ sMach . mCycle
  sExt . adviceMap . at cycle %= Just . (adv:) . maybe [] id

adviceHandler :: Regs r => Lens' s AdviceMap -> InstrHandler r s
adviceHandler advice (Istore w op2 r1) = do
  addr <- opVal op2
  (waddr, offset) <- lift $ splitAlignedAddr w addr
  storeVal <- regVal r1
  memVal <- use $ sMach . mMemWord waddr
  let memVal' = memVal & subBytes w offset .~ storeVal
  recordAdvice advice (MemOp addr memVal' MOStore w)
adviceHandler advice (Iload w _rd op2) = do
  addr <- opVal op2
  (waddr, _offset) <- lift $ splitAlignedAddr w addr
  val <- use $ sMach . mMemWord waddr
  recordAdvice advice (MemOp addr val MOLoad w)
adviceHandler advice (Ipoison w op2 r1) = do
  addr <- opVal op2
  val <- regVal r1
  recordAdvice advice (MemOp addr val MOPoison w)
adviceHandler _ _ = return ()


-- Trace handler (for debugging)

readStr :: Monad m => MWord -> InterpM r s m Text
readStr ptr = do
  let (waddr, offset) = splitAddr ptr
  mem <- use $ sMach . mMem . wmMem
  let firstWord = maybe 0 id $ mem ^? ix waddr
      firstBytes = drop offset $ splitWord firstWord
      restWords = [maybe 0 id $ mem ^? ix waddr' | waddr' <- [waddr + 1 ..]]
      bytes = takeWhile (/= 0) $ concat $ firstBytes : map splitWord restWords
      bs = BS.pack bytes
      t = Text.decodeUtf8With (\_ _ -> Just '?') bs
  return t
  where
    splitWord :: MWord -> [Word8]
    splitWord x = [fromIntegral $ x `shiftR` (i * 8) | i <- [0 .. wordBytes - 1]]

traceHandler :: Regs r => Bool -> InstrHandler r s -> InstrHandler r s
traceHandler active _nextH (Iext (XTrace desc ops)) = do
  vals <- mapM opVal ops
  when active $ traceM $ "TRACE[" ++ Text.unpack desc ++ "] " ++ intercalate ", " (map show vals)
  nextPc
traceHandler active _nextH (Iext (XTraceStr ptrOp)) = do
  ptr <- opVal ptrOp
  s <- readStr ptr
  when active $ traceM $ "TRACESTR " ++ Text.unpack s
  nextPc
traceHandler active _nextH (Iext (XTraceExec nameOp valOps)) = do
  namePtr <- opVal nameOp
  name <- readStr namePtr
  vals <- mapM opVal valOps
  let vals' = reverse $ dropWhile (== 0) $ reverse vals
  when active $ traceM $ "[FUNC] " ++ Text.unpack name ++
    "(" ++ intercalate ", " (map (drop 2 . showHex) vals') ++ ")"
  nextPc
traceHandler active nextH instr@(Ianswer op) = do
  val <- opVal op
  when active $ traceM $ "ANSWER = " ++ show val
  nextH instr
traceHandler _active nextH instr = nextH instr


-- Memory allocation tracking

data MemErrorKind = OutOfBounds | UseAfterFree | Unallocated
  deriving (Show, Eq)

data AllocState = AllocState
  -- | The next unused address.  Used for future allocations.
  { _asFrontier :: MWord
  -- | Map tracking all memory that is valid to access at the moment.  An entry
  -- `(k, v)` means that addresses `k <= addr < v` are valid to access.
  -- Entries are all non-empty (`k < v`) and non-overlapping.
  , _asValid :: Map MWord MWord
  -- | The address of each `malloc` performed by the program.  This is passed
  -- to the second run so it can replicate the same `malloc` behavior, without
  -- having to repeat the same allocation logic.  (Currently, the logic is
  -- simple - just a bump pointer - but it will become a bit more complex in
  -- the future to handle "use before alloc" errors, initial memory, and more
  -- efficient usage of the address space.)
  , _asMallocAddrs :: Seq MWord
  , _asMemErrors :: Seq (MemErrorKind, MWord)
  }

makeLenses ''AllocState

heapStart :: MWord
heapStart = 1 `shiftL` 32

initAllocState :: AllocState
initAllocState = AllocState heapStart initValid Seq.empty Seq.empty
  -- Initially, only the range 0 .. heapStart is valid.
  where initValid = Map.singleton 0 heapStart

redzoneSize :: MWord
redzoneSize = 128

ceilLog2 :: MWord -> Int
ceilLog2 0 = 1
ceilLog2 n = wordBits - countLeadingZeros (n - 1)

-- | Helper function for markValid and markInvalid.  Deletes all entries with
-- key `start <= k < end` from `valid`.
validDeleteRange :: MWord -> MWord -> Map MWord MWord -> Map MWord MWord
validDeleteRange start end valid = go valid
  where
    -- Find the first entry with `start <= k`, and if `k < end`, delete it.
    go m = case Map.lookupGE start m of
      Just (k, _) | k < end -> go $ Map.delete k m
      _ -> m

markValid :: MWord -> MWord -> Map MWord MWord -> Map MWord MWord
markValid start end valid | end <= start = valid
markValid start end valid =
    Map.insert start' end' $ validDeleteRange start' end' valid
  where
    -- If the range `start .. end` overlaps some ranges already in the map,
    -- extend to cover those ranges as well.
    start' = case Map.lookupLE start valid of
      Just (prevStart, prevEnd) | prevStart < start && start <= prevEnd -> prevStart
      _ -> start
    end' = case Map.lookupLE end valid of
      Just (nextStart, nextEnd) | nextStart <= end && end < nextEnd -> nextEnd
      _ -> end

markInvalid :: MWord -> MWord -> Map MWord MWord -> Map MWord MWord
markInvalid start end valid | end <= start = valid
markInvalid start end valid =
    validDeleteRange start end $ splitAt start $ splitAt end $ valid
  where
    -- If an entry in the map overlaps `pos`, split the entry in two so that
    -- `pos` falls on the boundary.
    splitAt pos m = case Map.lookupLE pos m of
      Just (start, end) | start < pos && pos < end ->
        Map.union (Map.fromList [(start, pos), (pos, end)]) $ Map.delete start m
      _ -> m


-- | Check the allocation status of `addr`, and record a memory error if it
-- isn't valid.
checkAccess :: Regs r => Bool -> Lens' s AllocState -> MWord -> InterpM' r v s Hopefully ()
checkAccess verbose allocState addr = do
  validMap <- use $ sExt . allocState . asValid
  let valid = case Map.lookupLE addr validMap of
        Just (start, end) -> start <= addr && addr < end
        Nothing -> False
  when (not valid) $ do
    -- TODO: update `asValid` to keep track of why a given region is invalid,
    -- so we can produce a more precise message and MemErrorKind
    when verbose $ traceM $ "detected bad access at " ++ showHex addr
    sExt . allocState . asMemErrors  %= (Seq.|> (OutOfBounds, addr))

allocHandler :: Regs r => Bool -> Lens' s AllocState -> InstrHandler r s -> InstrHandler r s
allocHandler verbose allocState _nextH (Iextadvise rd (XMalloc sizeOp)) = do
  size <- opVal sizeOp
  let sizeClass = ceilLog2 size
  let size' = 1 `shiftL` sizeClass

  base <- use $ sExt . allocState . asFrontier
  -- Start at `base + redzoneSize`, then round up to the next multiple of size'
  let addr = (base + redzoneSize + size' - 1) .&. complement (size' - 1)
  sExt . allocState . asFrontier .= addr + size'

  let ptr = addr .|. (fromIntegral sizeClass `shiftL` 58)
  sExt . allocState . asMallocAddrs %= (Seq.|> ptr)

  when verbose $ traceM $ "malloc " ++ show size ++ " bytes (extended to " ++ show size' ++
    ") at " ++ showHex ptr

  sMach . mReg rd .= ptr
  finishInstr
allocHandler verbose allocState _nextH (Iext (XAccessValid loOp hiOp)) = do
  lo <- opVal loOp
  hi <- opVal hiOp
  sExt . allocState . asValid %= markValid lo hi
  when verbose $ traceM $ "valid: " ++ showHex lo ++ " .. " ++ showHex hi
  finishInstr
allocHandler verbose allocState _nextH (Iext (XAccessInvalid loOp hiOp)) = do
  lo <- opVal loOp
  hi <- opVal hiOp
  sExt . allocState . asValid %= markInvalid lo hi
  when verbose $ traceM $ "invalid: " ++ showHex lo ++ " .. " ++ showHex hi
  finishInstr
allocHandler _verbose _allocState _nextH (Iextadvise rd (XAdvisePoison _lo _hi)) = do
  -- Always return 0 (don't poison)
  sMach . mReg rd .= 0
  finishInstr
allocHandler verbose  allocState nextH instr@(Istore _w op2 _r1) = do
  addr <- opVal op2
  -- TODO: this misses errors when the first byte of the access is in bounds
  -- but the later bytes are not.  However, we can't catch such errors yet,
  -- since we can't poison only part of a word.
  checkAccess verbose allocState addr
  nextH instr
allocHandler verbose allocState nextH instr@(Iload _w _rd op2) = do
  addr <- opVal op2
  checkAccess verbose allocState addr
  nextH instr
allocHandler _ _ _ (Iextval rd (XLoadUnchecked op2)) = do
  addr <- opVal op2
  val <- doLoad WWord addr
  sMach . mReg rd .= val
  finishInstr
allocHandler _ _ _ (Iext (XStoreUnchecked op2 op1)) = do
  addr <- opVal op2
  val <- opVal op1
  doStore WWord addr val
  finishInstr
allocHandler _ _ nextH instr = nextH instr

-- Memory handling, second pass

data MemInfo = MemInfo
  { _miMallocAddrs :: Seq MWord
  , _miPoisonAddrs :: Set MWord
  }
makeLenses ''MemInfo

doAdvise :: Regs r => Lens' s AdviceMap -> r -> MWord -> InterpM r s Hopefully ()
doAdvise advice rd val = do
  recordAdvice advice (Advise val)
  sMach . mReg rd .= val

memErrorHandler :: Regs r => Lens' s MemInfo -> Lens' s AdviceMap ->
  InstrHandler r s -> InstrHandler r s
memErrorHandler info advice _nextH (Iextadvise rd (XMalloc _size)) = do
  val <- maybe (assumptError "ran out of malloc addrs") return =<<
    use (sExt . info . miMallocAddrs . to (Seq.lookup 0))
  sExt . info . miMallocAddrs %= Seq.drop 1
  doAdvise advice rd val
  finishInstr
memErrorHandler info advice _nextH (Iextadvise rd (XAdvisePoison loOp hiOp)) = do
  lo <- opVal loOp
  hi <- opVal hiOp
  addrs <- use $ sExt . info . miPoisonAddrs
  -- Find an address where the entire word at `addr` fits within `lo .. hi`.
  case Set.lookupGE lo addrs of
    Just addr | addr + fromIntegral wordBytes <= hi -> do
      doAdvise advice rd addr
      -- Poisoning a second time would be a prover error.
      sExt . info . miPoisonAddrs %= Set.delete addr
    _ -> do
      doAdvise advice rd 0
  finishInstr
memErrorHandler _info _advice nextH instr = nextH instr


-- Top-level interpreter

observer :: InstrHandler r s -> InstrHandler r s -> InstrHandler r s
observer obs nextH instr = obs instr >> nextH instr

execTraceHandler :: Regs r =>
  Lens' s (Seq (ExecutionState r)) ->
  Lens' s AdviceMap ->
  InstrHandler r s -> InstrHandler r s
execTraceHandler eTrace eAdvice nextH instr = do
  nextH instr
  s <- getStateWithAdvice eAdvice
  sExt . eTrace %= (Seq.|> s)

runWith :: Regs r => InstrHandler r s -> Word -> InterpState r s -> Hopefully (InterpState r s)
runWith handler steps initState = evalStateT go initState
  where
    go = do
      goSteps steps
      get

    goSteps 0 = return ()
    goSteps n = do
      goStep
      ans <- use $ sMach . mAnswer
      case ans of
        Just _ -> return ()
        Nothing -> goSteps (n - 1)

    goStep = do
      pc <- use $ sMach . mPc
      i <- fetchInstr pc
      handler i

runPass1 :: Regs r => Bool -> Word -> MachineState r -> Hopefully MemInfo
runPass1 verbose steps initMach' = do
  final <- runWith handler steps initState
  return $ getMemInfo $ final ^. sExt
  where
    initState = InterpState initAllocState initMach'
    handler = traceHandler verbose  $ allocHandler verbose id $ stepInstr

    getMemInfo :: AllocState -> MemInfo
    getMemInfo as = MemInfo (as ^. asMallocAddrs) (getPoisonAddrs $ as ^. asMemErrors)

    -- | Get a set of addresses of words to poison.  We need to provide several
    -- options for poisoning since some memory errors may not be poisonable,
    -- depending on where exactly the error occurs.
    getPoisonAddrs :: Seq (MemErrorKind, MWord) -> Set MWord
    getPoisonAddrs errs = Set.fromList
      [addr .&. complement (fromIntegral wordBytes - 1)
        | (kind, addr) <- toList errs, kind /= Unallocated]

runPass2 :: Regs r => Word -> MachineState r -> MemInfo -> Hopefully (Trace r)
runPass2 steps initMach' memInfo = do
  -- The first entry of the trace is always the initial state.  Then `steps`
  -- entries follow after it.
  initExecState <- evalStateT (getStateWithAdvice eAdvice) initState
  final <- runWith handler steps initState
  return $ initExecState : toList (final ^. sExt . eTrace)
  where
    initState = InterpState (Seq.empty, Map.empty, memInfo) initMach'

    eTrace :: Lens' (a, b, c) a
    eTrace = _1
    eAdvice :: Lens' (a, b, c) b
    eAdvice = _2
    eMemInfo :: Lens' (a, b, c) c
    eMemInfo = _3

    handler =
      execTraceHandler eTrace eAdvice $
      observer (adviceHandler eAdvice) $
      memErrorHandler eMemInfo eAdvice $
      traceHandler False $
      stepInstr


-- | Used for checking final traces after post porocessing
runPassGeneric :: Regs r => Lens' s (Seq (ExecutionState r)) -> Lens' s (AdviceMap) -> (InstrHandler r s -> InstrHandler r s)
                -> s -> Word -> MachineState r -> Hopefully (Trace r)
runPassGeneric eTrace eAdvice postHandler initS steps  initMach' = do
  -- The first entry of the trace is always the initial state.  Then `steps`
  -- entries follow after it.k
  initExecState <- evalStateT (getStateWithAdvice eAdvice) initState
  final <- runWith handler steps initState
  return $ initExecState : toList (final ^. sExt . eTrace)
  where
    initState = InterpState initS initMach'
    handler =
      execTraceHandler eTrace eAdvice $
      postHandler $
      stepInstr

-- Old public definitions

type Mem' = (MWord, Map MWord MWord)
type Poison = Set MWord

-- | The program state 
data ExecutionState mreg = ExecutionState {
  -- | Program counter
  pc :: MWord
  -- | Register bank
  , regs :: RegBank mreg MWord
  -- | Memory state
  , mem :: Mem'
  -- | Locations that have been poisoned
  , psn :: Poison
  -- | Nondeterministic advice for the last step
  , advice :: [Advice]
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
  WordMemory d m psn <- use $ sMach . mMem
  let mem = (d, m)
  cycle <- use $ sMach . mCycle
  -- Retrieve advice for the cycle that just finished executing.
  adv <- use $ sExt . advice . ix (cycle - 1)
  bug <- use $ sMach . mBug
  let inv = False
  answer <- use $ sMach . mAnswer
  return $ ExecutionState pc regs mem psn adv bug inv (maybe 0 id answer)

type Prog mreg = Program mreg MWord
type Trace mreg = [ExecutionState mreg]

initMach :: Regs r => Program r MWord -> InitialMem -> MachineState r
initMach prog imem = MachineState
  { _mCycle = 0
  , _mPc = 0
  , _mRegs = initBank (lengthInitMem imem)
  , _mProg = Seq.fromList prog
  , _mMem = absInitMem @MWord $ flatInitMem imem
  , _mBug = False
  , _mAnswer = Nothing
  }

type Executor mreg r = CompilationResult (Prog mreg) -> r
-- | Produce the trace of a program
run_v :: Regs mreg => Bool ->  Executor mreg (Trace mreg)
run_v verbose (CompUnit progs trLen _ _ _analysis _) = case go of
  Left e -> error $ describeError e
  Right x -> x
  where
    go = do
      let highState = initMach (pmProg $ highProg progs) (pmMem $ highProg progs)
      memInfo <- runPass1 verbose  (trLen - 1) highState
      let lowState = initMach (pmProg $ lowProg progs) (pmMem $ lowProg progs)
      tr <- runPass2 (trLen - 1) lowState memInfo
      return tr
      
run :: Regs mreg => Executor mreg (Trace mreg)
run = run_v False
      
-- | Execute the program and return the result.
execAnswer :: Regs mreg => Bool -> Executor mreg MWord
execAnswer verb compUnit = answer $ last $ run_v verb compUnit

-- | Execute the program and tells if there was a bug.
execBug :: Regs mreg => Bool -> Executor mreg Bool
execBug verb compUnit = bug_flag $ last $ run_v verb compUnit

-- | Read from a location in memory
load ::  MWord -> Mem' -> MWord
load x (d,m)=  case Map.lookup x m of
                 Just y -> y
                 Nothing -> d



