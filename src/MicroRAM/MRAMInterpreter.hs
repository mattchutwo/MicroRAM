{-# LANGUAGE PolyKinds #-}
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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    runWith, initMach, InstrHandler, runPassGeneric, InterpState,
    sMach, sCachedMach, sExt, mCycle, mPc,
    InstrHandler', InterpState',
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

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Lens (makeLenses, ix, at, to, (^.), over, (.=), (%=), use, Lens', _1, _2, _3)
import Data.Bits
import qualified Data.ByteString as BS
import Data.Foldable
import Data.List (intercalate)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq ( (:|>) ))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Vector (Vector)
import Data.Word

import GHC.Generics (Generic)


import Compiler.Errors
import Compiler.Registers
import Compiler.Tainted
import Compiler.CompilationUnit
import MicroRAM
import MicroRAM.MRAMInterpreter.Concrete
import MicroRAM.MRAMInterpreter.Generic
import MicroRAM.MRAMInterpreter.Tainted
import qualified Native

import Util.Util

-- import Debug.PrettyPrint
import Debug.Trace

-- AbsDomain instance for MWord (concrete interpreter)
type InterpState r s = InterpState' r MWord s
type InstrHandler r s = InstrHandler' r MWord s

-- Advice-generating extension

-- | Memory operation advice: nondeterministic advice to help the
-- witness checker chekc the memory consistency. 
data MemOpType = MOStore | MOLoad | MOPoison
  deriving (Eq, Read, Show, Generic)

-- | This information is passed the witness checker as nondeterministic
-- advice. Currently we only advice about memory operations and steps that stutter.
data Advice =
    MemOp
      MWord          -- ^ address
      MWord          -- ^ value
      MemOpType      -- ^ read or write
      MemWidth       -- ^ width of the access
      (Maybe (Vector Label)) -- ^ Labels for each byte

  | Advise MWord
  | Stutter
  deriving (Eq, Read, Show, Generic)

concretizeAdvice :: forall v. Concretizable v
                 => v -- ^ address
                 -> v -- ^ abstract value
                 -> MemOpType
                 -> MemWidth
                 -> Advice
concretizeAdvice addr val t w =
  MemOp (conGetValue addr) (conGetValue val) t w (conGetTaint val)

-- | Pretty printer for advice.
renderAdvc :: [Advice] -> String
renderAdvc advs = concat $ map renderAdvc' advs
  where renderAdvc' :: Advice -> String
        renderAdvc' (MemOp addr v MOStore _w l) = "Store: " ++ show addr ++ "->" ++ show v   ++ " (" ++ show l ++ ")"
        renderAdvc' (MemOp  addr v MOLoad _w l) = "Load: " ++ show addr ++ "->" ++ show v    ++ " (" ++ show l ++ ")"
        renderAdvc' (MemOp addr v MOPoison _w l) = "Poison: " ++ show addr ++ "->" ++ show v ++ " (" ++ show l ++ ")"
        renderAdvc' (Advise v) = "Advise: " ++ show v
        renderAdvc' (Stutter) = "...Stutter..."

-- Returns address aligned to the word 
conAlignWord :: forall v. Concretizable v => v -> MWord
conAlignWord v = conGetValue v `shiftR` logWordBytes `shiftL` logWordBytes

regBankValues :: Concretizable v => RegBank r v -> RegBank r MWord
regBankValues (RegBank m dflt) = RegBank (Map.map conGetValue m) (conGetValue dflt)

regBankTaint :: Concretizable v => RegBank r v -> Maybe (RegBank r (Vector Label))
regBankTaint (RegBank m dflt) =
  case (conGetTaint dflt) of
    Nothing -> Nothing
    Just dfltTnt -> Just $ RegBank (Map.mapMaybe conGetTaint m) dfltTnt

regBankSplit :: Concretizable v => RegBank r v -> (RegBank r MWord, Maybe (RegBank r (Vector Label)))
regBankSplit rb = (regBankValues rb, regBankTaint rb)

instance Concretizable MWord where
  conGetValue = id
  conGetTaint _ = Nothing 
  conMem (WordMemory d m _) =  (d, m, Nothing)
  conPoison (WordMemory _ _ psn) = psn
  
  
instance Concretizable TaintedValue where
  conGetValue (TaintedValue w _) = w
  conGetTaint (TaintedValue _ t) = Just t
  conMem (TaintedMem d m l _) = (d, m, Just l)
  conPoison (TaintedMem _ _ _ psn) = psn
    
type AdviceMap = Map MWord [Advice]

recordAdvice' :: (AbsDomain v, Regs r) =>
  Lens' s AdviceMap -> Bool -> Advice -> InterpM' r v s Hopefully ()
recordAdvice' adviceMap isPost adv = do
  cycle <- use $ sMach . mCycle
  cycleWord <- lift $ tag "Failed at making the cycle concrete, while recording advice" $
    absGetValue cycle
  let cycleWord' = if isPost then cycleWord - 1 else cycleWord
  sExt . adviceMap . at cycleWord' %= Just . (adv:) . maybe [] id

-- | Helper function to record advice.  This is used by `adviceHandler` and
-- also by any other handlers that want to record advice of their own.
recordAdvice :: (AbsDomain v, Regs r) =>
  Lens' s AdviceMap -> Advice -> InterpM' r v s Hopefully ()
recordAdvice adviceMap adv = recordAdvice' adviceMap False adv

-- | Like `recordAdvice`, but it can be used in the post state, after running
-- the normal instruction.
recordAdvicePost :: (AbsDomain v, Regs r) =>
  Lens' s AdviceMap -> Advice -> InterpM' r v s Hopefully ()
recordAdvicePost adviceMap adv = recordAdvice' adviceMap True adv

adviceHandler :: forall v r s. (Concretizable v, Regs r) =>
  Lens' s AdviceMap -> InstrHandler' r v s -> InstrHandler' r v s
adviceHandler advice nextH instr@(Istore w op2 _r1) = do
  addr <- opVal op2
  -- The advide records the entire Word with proper alignment.
  let waddr = conAlignWord addr 
  -- Run `nextH` before `doLoad` so the `doLoad` will observe the store.
  nextH instr
  storedVal <- doLoad WWord (absExact waddr)
  recordAdvicePost advice $ concretizeAdvice addr storedVal MOStore w
adviceHandler advice nextH instr@(Iload w _rd op2) = do
  addr <- opVal op2
  -- The advide records the entire Word with proper alignment.
  let waddr = conAlignWord addr
  loadedVal <- doLoad WWord (absExact waddr)
  recordAdvice advice $ concretizeAdvice addr loadedVal MOLoad w
  -- Run `nextH` only after computing `addr`, since if `rd == op2`, `nextH`
  -- will modify the result of `opVal op2`.
  nextH instr
adviceHandler advice nextH instr@(Ipoison w op2 r1) = do
  addr <- opVal op2
  val <- regVal r1
  recordAdvice advice $ concretizeAdvice addr val MOPoison w
  nextH instr
adviceHandler _ nextH instr = nextH instr


-- | Trace handler (for debugging)
-- This function is written for any AbsDomain,
-- But it assumes that it has values (i.e. `absGetValue` succeeds)
-- for the entire string to be read, including the entire last word
-- that contains part of the string, the ending symbol `0` and possibly
-- some extra garbage.
readStr :: forall v r s. Concretizable v => v -> InterpM' r v s Hopefully Text
readStr ptr = do
  let ptr' = conGetValue  ptr
  mem :: Memory v <- use $ sMach . mMem
  (_, str, _) <- lift $ iterateUntilM isDone (readNextByte mem) (ptr', [], False)
  let bs = BS.pack str
      t = Text.decodeUtf8With (\_ _ -> Just '?') bs
  return t
  where
    -- | Marks when the execution finds the end of the string
    -- marked by a '0' byte
    isDone (_,_,done) = done

    -- | Reads the next byte/char in a string.
    -- If the byte is 0 it only sets `isDone` to true.
    -- The "state" of the execution is a triple containing:
    -- * MWord   -- The next address to read.
    -- * [Word8] -- The accumulated string read so far
    -- * Bool    -- If the end of the string has been found.
    readNextByte :: Memory v -> (MWord, [Word8], Bool) -> Hopefully (MWord, [Word8], Bool)
    readNextByte mem (addr, str, _) = do
      nextByte <- readByte addr mem
      let nextAddr = addr + 1
      return $ if nextByte == 0 then
                 (nextAddr, str, True)
               else
                 (nextAddr, str ++ [nextByte], False)

    -- | In this function, `absLoad W1` insures that the word is always
    -- smaller than the largest byte, so the conversion is legal
    readByte :: MWord -> Memory v -> Hopefully Word8
    readByte addr mem = fromIntegral . conGetValue <$> absLoad W1 (absExact @v addr) mem
    
-- This assumes that XSnapshot and XCheck surround a native instruction.
snapshotHandler :: (Concretizable v, Regs r) => InstrHandler' r v s -> InstrHandler' r v s
snapshotHandler _nextH (Iext XSnapshot) = do
  m <- use sMach 
  sCachedMach .= Just (over mPc succ m) 
  sCachedInstrs .= Seq.Empty
  nextPc
snapshotHandler _nextH (Iext (XCheck (Native.NativeInstruction i) blockName offset)) = do

  -- Get MicroRAM state (after the intepreter already took the steps).
  mramState <- _sMach <$> get

  -- Convert to native architecture and then take a step.
  initStateM <- _sCachedMach <$> get
  case initStateM of
    Nothing ->
      otherError $ "No cached machine state for XCheck"
    Just initState -> do
      instrs <- use sCachedInstrs
      cycle <- conGetValue <$> (use $ sMach . mCycle)
      traceM $ "cycle " ++ show cycle ++ ", block " ++ show blockName ++ " +" ++ show offset ++
        ": simulate " ++ show i ++ " = " ++ show instrs
      let archState = Native.stepArch (Native.toArchState initState) i

      pc <- use (sMach . mPc)
      count <- maybe 0 id <$> use (sCheckCount . at pc)

      -- Simulation check that toArch (step i) == step (toArch i).
      case archState of
        _ | count >= 5 -> nextPc
        Right r | Native.archStateEq (Native.toArchState mramState) r -> do
          sCheckCount %= Map.insert pc (count + 1)
          nextPc
        Left e -> do
          traceM $ "warning: unhandled riscv instruction " ++ show i ++ " (" ++ describeError e ++ ")"
          nextPc
        _ -> do
          instrs <- use sCachedInstrs
          otherError $ "[CHECK] Native Simulation failed. Steps didn't match." <>
            "\nMRAM Instructions:\n\t" <> show (instrs) <>
            "\nRiscV Instruction:\n\t" <> show i <>
            "\nThe initial state:\n " <> (prettyPrintMachState initState) <>
            "\nThe final state  :\n " <> (prettyPrintMachState mramState)
            
snapshotHandler nextH instr = do
  sCachedInstrs %= (\seq -> seq :|> instr)  
  nextH instr


traceHandler :: (Concretizable v, Regs r) => Bool -> InstrHandler' r v s -> InstrHandler' r v s
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
  vals <- mapM (conGetValue <.> opVal) valOps
  let vals' = reverse $ dropWhile (== 0) $ reverse vals
  when active $ traceM $ "[FUNC] " ++ Text.unpack name ++
    "(" ++ intercalate ", " (map (drop 2 . showHex) vals') ++ ")"
  nextPc
traceHandler active nextH instr@(Ianswer op) = do
  val <- opVal op
  when active $ traceM $ "ANSWER = " ++ show val
  cycle <- conGetValue <$> (use $ sMach . mCycle)
  when active $ traceM $ "Terminating on cycle " ++ show cycle
  nextH instr
traceHandler _active nextH instr = nextH instr


-- Memory allocation tracking

data MemErrorKind = OutOfBounds | UseAfterFree | Unallocated
  deriving (Show, Eq)

data AllocState = AllocState
  {
  -- | The next unused address.  Used for future allocations.
  _asFrontier :: MWord
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
checkAccess :: (Concretizable v, Regs r) => Bool -> Lens' s AllocState -> MWord -> InterpM' r v s Hopefully ()
checkAccess verbose allocState addr = do
  validMap <- use $ sExt . allocState . asValid
  let valid = case Map.lookupLE addr validMap of
        Just (start, end) -> start <= addr && addr < end
        Nothing -> False
  when (not valid) $ do
    -- TODO: update `asValid` to keep track of why a given region is invalid,
    -- so we can produce a more precise message and MemErrorKind
    cycle <- conGetValue <$> (use $ sMach . mCycle)
    when verbose $ traceM $ "Cycle: "++ show cycle ++". Detected bad access at " ++ showHex addr
    sExt . allocState . asMemErrors  %= (Seq.|> (OutOfBounds, addr))

allocHandler :: forall v r s. (Concretizable v, Regs r) => Bool -> Lens' s AllocState -> InstrHandler' r v s -> InstrHandler' r v s
allocHandler verbose allocState _nextH (Iextadvise rd _ (XMalloc sizeOp)) = do
  size <- conGetValue <$> opVal sizeOp
  let sizeClass = ceilLog2 size
  let size' = 1 `shiftL` sizeClass

  base <- use $ sExt . allocState . asFrontier
  -- Start at `base + redzoneSize`, then round up to the next multiple of size'
  let addr = (base + redzoneSize + size' - 1) .&. complement (size' - 1)
  sExt . allocState . asFrontier .= addr + size'

  let ptr =  addr .|. (fromIntegral sizeClass `shiftL` 58)
  sExt . allocState . asMallocAddrs %= (Seq.|> ptr)

  when verbose $ traceM $ "malloc " ++ show size ++ " bytes (extended to " ++ show size' ++
    ") at " ++ showHex ptr

  sMach . mReg rd .= absExact ptr
  finishInstr
  
allocHandler verbose allocState _nextH (Iext (XAccessValid loOp hiOp)) = do
  lo <- conGetValue <$> opVal loOp
  hi <- conGetValue <$> opVal hiOp
  sExt . allocState . asValid %= markValid lo hi
  when verbose $ traceM $ "valid: " ++ showHex lo ++ " .. " ++ showHex hi
  finishInstr
allocHandler verbose allocState _nextH (Iext (XAccessInvalid loOp hiOp)) = do
  lo <- conGetValue <$> opVal loOp
  hi <- conGetValue <$> opVal hiOp
  sExt . allocState . asValid %= markInvalid lo hi
  when verbose $ traceM $ "invalid: " ++ showHex lo ++ " .. " ++ showHex hi
  finishInstr
allocHandler _verbose _allocState _nextH (Iextadvise rd maxValOp (XAdvisePoison _lo _len)) = do
  -- Always return maxVal (don't poison)
  maxVal <- conGetValue <$> opVal maxValOp
  sMach . mReg rd .= absExact maxVal
  finishInstr
allocHandler verbose  allocState nextH instr@(Istore _w op2 _r1) = do
  addr <- conGetValue <$>  opVal op2
  -- TODO: this misses errors when the first byte of the access is in bounds
  -- but the later bytes are not.  However, we can't catch such errors yet,
  -- since we can't poison only part of a word.
  checkAccess verbose allocState addr
  nextH instr
allocHandler verbose allocState nextH instr@(Iload _w _rd op2) = do
  addr <- conGetValue <$> opVal op2
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

doAdvise :: forall r s v. (AbsDomain v, Regs r) => Lens' s AdviceMap -> r -> MWord -> InterpM' r v s Hopefully ()
doAdvise advice rd val = do
  recordAdvice advice (Advise val)
  sMach . mReg rd .= absExact val

memErrorHandler :: forall r s v. (Concretizable v, Regs r) => Lens' s MemInfo -> Lens' s AdviceMap ->
  InstrHandler' r v s -> InstrHandler' r v s
memErrorHandler info advice _nextH (Iextadvise rd _ (XMalloc _size)) = do
  val <- maybe (assumptError "ran out of malloc addrs") return =<<
    use (sExt . info . miMallocAddrs . to (Seq.lookup 0))
  sExt . info . miMallocAddrs %= Seq.drop 1
  doAdvise advice rd val
  finishInstr
memErrorHandler info advice _nextH (Iextadvise rd _ (XAdvisePoison loOp lenOp)) = do
  lo <- conGetValue <$> opVal loOp
  len <- conGetValue <$> opVal lenOp
  let hi = lo + len
  addrs <- use $ sExt . info . miPoisonAddrs
  -- Find an address where the entire word at `addr` fits within `lo .. hi`.
  case Set.lookupGE lo addrs of
    Just addr | addr + fromIntegral wordBytes <= hi -> do
      doAdvise advice rd (addr - lo)
      -- Poisoning a second time would be a prover error.
      sExt . info . miPoisonAddrs %= Set.delete addr
    _ -> do
      doAdvise advice rd (hi - lo)
  finishInstr
memErrorHandler _info _advice nextH instr = nextH instr


-- Top-level interpreter

execTraceHandler :: (Concretizable v, Regs r) =>
  Lens' s (Seq (ExecutionState r)) ->
  Lens' s AdviceMap ->
  InstrHandler' r v s -> InstrHandler' r v s
execTraceHandler eTrace eAdvice nextH instr = do
  nextH instr
  s <- getStateWithAdvice eAdvice
  sExt . eTrace %= (Seq.|> s)

runWith :: Regs r => InstrHandler' r v s -> Word -> InterpState' r v s -> Hopefully (InterpState' r v s)
runWith handler steps initState = execStateT (goSteps steps) initState
  where
    goSteps 0 = return ()
    goSteps n = do
      goStep
      ans <- use $ sMach . mAnswer
      case ans of
        Just _ -> return () -- trace ("ANSWER: " <> show ans <> ". Step : " <> show n) $ return ()
        Nothing -> goSteps (n - 1)

    goStep = do
      pc <- use $ sMach . mPc
      i <- fetchInstr pc
      handler i

runPass1 :: (Concretizable v, Regs r) => Bool -> Word -> MachineState' r v -> Hopefully MemInfo
runPass1 verbose steps initMach' = do
  final <- runWith handler steps initState
  return $ getMemInfo $ final ^. sExt
  where
    initState = InterpState initAllocState initMach' Nothing Seq.Empty mempty
    handler = snapshotHandler $ traceHandler verbose $ allocHandler verbose id $ stepInstr

    getMemInfo :: AllocState -> MemInfo
    getMemInfo as = MemInfo (as ^. asMallocAddrs) (getPoisonAddrs $ as ^. asMemErrors)

    -- | Get a set of addresses of words to poison.  We need to provide several
    -- options for poisoning since some memory errors may not be poisonable,
    -- depending on where exactly the error occurs.
    getPoisonAddrs :: Seq (MemErrorKind, MWord) -> Set MWord
    getPoisonAddrs errs = Set.fromList
      [addr .&. complement (fromIntegral wordBytes - 1)
        | (kind, addr) <- toList errs, kind /= Unallocated]

runPass2 :: (Concretizable v, Regs r) => Word -> MachineState' r v -> MemInfo -> Hopefully (Trace r)
runPass2 steps initMach' memInfo = do
  -- The first entry of the trace is always the initial state.  Then `steps`
  -- entries follow after it.
  initExecState <- evalStateT (getStateWithAdvice eAdvice) initState
  final <- runWith handler steps initState
  return $ initExecState : toList (final ^. sExt . eTrace)
  where
    initState = InterpState (Seq.empty, Map.empty, memInfo) initMach' Nothing Seq.Empty mempty

    eTrace :: Lens' (a, b, c) a
    eTrace = _1
    eAdvice :: Lens' (a, b, c) b
    eAdvice = _2
    eMemInfo :: Lens' (a, b, c) c
    eMemInfo = _3

    handler =
      execTraceHandler eTrace eAdvice $
      adviceHandler eAdvice $
      memErrorHandler eMemInfo eAdvice $
      traceHandler False $
      stepInstr


-- | Used for checking final traces after post porocessing
runPassGeneric :: forall r s v. (Concretizable v, Regs r)
               => Lens' s (Seq (ExecutionState r))
               -> Lens' s AdviceMap
               -> (InstrHandler' r v s -> InstrHandler' r v s)
               -> s -> Word -> MachineState' r v -> Hopefully (Trace r)
runPassGeneric eTrace eAdvice postHandler initS steps  initMach' = do
  -- The first entry of the trace is always the initial state.  Then `steps`
  -- entries follow after it.
  initExecState <- evalStateT (getStateWithAdvice eAdvice) initState
  final <- runWith handler steps initState
  return $ initExecState : toList (final ^. sExt . eTrace)
  where
    initState = InterpState initS initMach' Nothing Seq.Empty mempty
    handler =
      execTraceHandler eTrace eAdvice $
      postHandler $
      stepInstr

-- Old public definitions

-- | The program state 
data ExecutionState mreg = ExecutionState {
  -- | Program counter
  pc :: MWord
  -- | Register bank
  , regs :: RegBank mreg MWord
  -- | Register label bank
  , regLabels :: Maybe (RegBank mreg (Vector Label)) -- In the future, instead of dynamically tracking whether the tainted flag is provided, we could use the type system to ensure labels are tracked only when the tainted flag is provided.
  -- | Memory state
  , mem :: Mem
  -- | Locations that have been poisoned
  , psn :: Poison
  -- | Nondeterministic advice for the last step
  , advice :: [Advice]
  -- | Marks for bugs and invalid traces
  , bug_flag, inv_flag :: Bool
  -- | Return value.
  , answer :: MWord }

deriving instance (Ord mreg, Read mreg, Read (RegBank mreg MWord), Read (RegBank mreg (Vector Label))) => Read (ExecutionState mreg)
deriving instance (Show mreg, Show (RegBank mreg MWord), Show (RegBank mreg Label)) => Show (ExecutionState mreg)

getStateWithAdvice :: forall r v s m. (Concretizable v, Monad m) => Lens' s AdviceMap -> InterpM' r v s m (ExecutionState r)
getStateWithAdvice advice = do
  pc <- use $ sMach . mPc
  regsWithTaint <- use $ sMach . mRegs
  let (regs, regTnt) = regBankSplit regsWithTaint   
  absMem <- use $ sMach . mMem
  let mem = conMem @v absMem
      psn = conPoison @v absMem
  cycle <- conGetValue <$> (use $ sMach . mCycle)
  -- Retrieve advice for the cycle that just finished executing.
  (adv::[Advice]) <- use $ sExt . advice . ix (cycle - 1)
  bug <- use $ sMach . mBug
  let inv = False
  answer <- use $ sMach . mAnswer
  return $ ExecutionState pc regs regTnt mem psn adv bug inv (maybe 0 id answer)

type Prog mreg = Program mreg MWord
type Trace mreg = [ExecutionState mreg]
  
initMach :: forall v r. (AbsDomain v, Regs r) => Program r MWord -> InitialMem -> MachineState' r v
initMach prog imem = MachineState
          { _mCycle = absExact 0
          , _mPc = 0
          , _mRegs = initBank (absExact $ lengthInitMem imem) (absExact 0)
          , _mProg = Seq.fromList prog
          , _mMem = absInitMem @v imem
          , _mBug = False
          , _mAnswer = Nothing
          }

type Executor mreg r = CompilationResult (Prog mreg) -> r
-- | Produce the trace of a program
run_v :: Regs mreg => Bool -> Bool -> Executor mreg (Trace mreg)
run_v verbose leakTainted compUnit =
  case (if leakTainted then run' @TaintedValue else run' @MWord) verbose compUnit of
    Left e -> error $ describeError e
    Right x -> x
  where
    run'  :: forall v mreg. (Concretizable v, Regs mreg) => Bool -> Executor mreg (Hopefully (Trace mreg))
    run' verbose (CompUnit progs trLen _ _ _analysis _) = do
      let highState = initMach @v (pmProg $ highProg progs) (pmMem $ highProg progs)
      memInfo <- runPass1 verbose  (trLen - 1) highState
      let lowState = initMach @v (pmProg $ lowProg progs) (pmMem $ lowProg progs)
      tr <- runPass2 (trLen - 1) lowState memInfo
      return tr    
      
run :: Regs mreg => Bool -> Executor mreg (Trace mreg)
run leakTainted = run_v False leakTainted
      
-- | Execute the program and return the result.
execAnswer :: Regs mreg => Bool -> Bool -> Executor mreg MWord
execAnswer verb leakTainted compUnit = answer $ last $ run_v verb leakTainted compUnit

-- | Execute the program and tells if there was a bug.
execBug :: Regs mreg => Bool -> Bool -> Executor mreg Bool
execBug verb leakTainted compUnit = bug_flag $ last $ run_v verb leakTainted compUnit

-- | Read from a location in memory
load ::  MWord -> Mem -> MWord
load x (d,m,_l)=  case Map.lookup x m of
                 Just y -> y
                 Nothing -> d



