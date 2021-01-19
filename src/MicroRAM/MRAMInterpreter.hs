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
    Executor,
    run, run_v, execAnswer, execBug,
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
import Control.Lens (makeLenses, ix, at, to, lens, (^.), (.=), (%=), use, Lens', _1, _2, _3)
import Data.Bits
import Data.Foldable
import Data.List (intercalate)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Set (Set, member)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as Text

import GHC.Generics (Generic)

import Compiler.Sparsity
import Compiler.Analysis
import Compiler.Errors
import Compiler.Registers
import Compiler.CompilationUnit
import MicroRAM
import Util.Util

import Debug.Trace

type Poison = Set MWord
data Mem = Mem MWord (Map MWord MWord)

data MachineState r = MachineState
  { _mCycle :: MWord
  , _mPc :: MWord
  , _mRegs :: RegBank r MWord
  , _mFlag :: Bool
  , _mProg :: Seq (Instruction r MWord)
  , _mMem :: Mem
  , _mPsn :: Poison
  , _mBug :: Bool
  , _mAnswer :: Maybe MWord
  }  
makeLenses ''MachineState

data SparsityState = SparsityState
  { _spSpars :: Sparsity
  , _spLastSeen :: Map.Map InstrKind Int
  }
makeLenses ''SparsityState

initSparsSt :: Sparsity -> SparsityState
initSparsSt spars = SparsityState spars Map.empty
  
data InterpState r s = InterpState
  { _sExt :: s
  , _sMach :: MachineState r
  , _sSpars :: SparsityState
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
    Iand rd r1 op2 -> stepBinary (.&.) rd r1 op2
    Ior rd r1 op2 -> stepBinary (.|.) rd r1 op2
    Ixor rd r1 op2 -> stepBinary xor rd r1 op2
    Inot rd op2 -> stepUnary complement rd op2
    
    Iadd rd r1 op2 -> stepBinary (+) rd r1 op2
    Isub rd r1 op2 -> stepBinary (-) rd r1 op2
    Imull rd r1 op2 -> stepBinary (*) rd r1 op2
    Iumulh rd r1 op2 -> stepBinary umulh rd r1 op2
    Ismulh rd r1 op2 -> stepBinary smulh rd r1 op2
    -- TODO div/mod vs quot/rem?
    Iudiv rd r1 op2 -> stepBinary safeDiv rd r1 op2
    Iumod rd r1 op2 -> stepBinary safeMod rd r1 op2
    
    Ishl rd r1 op2 -> stepBinary shiftL' rd r1 op2
    Ishr rd r1 op2 -> stepBinary shiftR' rd r1 op2
    
    Icmpe r1 op1 op2 -> stepCompare (==) r1 op1 op2
    Icmpa r1 op1 op2 -> stepCompare (>) r1 op1 op2
    Icmpae r1 op1 op2 -> stepCompare (>=) r1 op1 op2
    Icmpg r1 op1 op2 -> stepCompare signedGt r1 op1 op2
    Icmpge r1 op1 op2 -> stepCompare signedGe r1 op1 op2
    
    Imov rd op2 -> stepMove rd (Const 1) op2
    Icmov rd op1 op2 -> stepMove rd (Reg op1) op2
    
    Ijmp op2 -> stepJump (Const 1) True op2
    Icjmp r2 op2 -> stepJump (Reg r2) True op2
    Icnjmp r2 op2 -> stepJump (Reg r2) False op2
    
    Istore w op2 r1 -> stepStore w op2 r1
    Iload w rd op2 -> stepLoad w rd op2
    
    Iread rd op2 -> stepRead rd op2
    Ianswer op2  -> stepAnswer op2
    Ipoison w op2 r1 -> stepStore w op2 r1 >> poison w op2
    
    Iadvise _ -> assumptError $ "unhandled advice request"
    
    Iext name _ -> assumptError $ "unhandled extension instruction " ++ show name
    Iextval name _ _ -> assumptError $ "unhandled extension instruction " ++ show name
    Iextadvise name _ _ -> assumptError $ "unhandled extension instruction " ++ show name

  sMach . mCycle %= (+ 1)
  

-- ## Sparsity

controlSparsity :: Regs r
                => Lens' s AdviceMap
                -> InstrHandler r s
                -> InstrHandler r s
controlSparsity adviceMap step instr = do 
  stutter <- checkSparsity instr
  if stutter then do
    recordAdvice adviceMap Stutter
    sMach . mCycle %= (+ 1)
  else do
    step instr
checkSparsity :: Regs r => Instruction r MWord -> InterpM r s Hopefully Bool
checkSparsity instr = do
  kinds <- return $ instrType instr
  stutters <- mapM sparsity kinds 
  return $ or stutters 
  where sparsity :: Regs r => InstrKind -> InterpM r s Hopefully Bool
        sparsity kind = do
          cyc <- use $ sMach . mCycle
          lastMap <- use $ sSpars . spLastSeen
          last <- return $ Map.findWithDefault (-1) kind lastMap
          sSpars . spLastSeen %= (Map.insert kind (fromEnum cyc))
          sparsity <- use $ sSpars . spSpars
          targetSpars <- return $ Map.lookup kind sparsity
          case targetSpars of
            Nothing  -> return False
            Just spc -> do
              if sameFunctionalUnit spc last (fromEnum cyc)
                then return True
                else return False
  
        -- | Checks if two instances of the same kind would share the same functional Unit
        -- There is one Functional unit every 'spc' cycles.
        -- Last seen default is -1 so must use `div` 
        sameFunctionalUnit spc last cyc = (last `div` spc) == (cyc `quot` spc)

stepUnary :: Regs r => (MWord -> MWord) ->
  r -> Operand r MWord -> InterpM r s Hopefully ()
stepUnary f rd op2 = do
  y <- opVal op2
  let result = f y
  sMach . mReg rd .= result
  nextPc

stepBinary :: Regs r => (MWord -> MWord -> MWord) ->
  r -> r -> Operand r MWord -> InterpM r s Hopefully ()
stepBinary f rd r1 op2 = do
  x <- regVal r1
  y <- opVal op2
  let result = f x y
  sMach . mReg rd .= result
  nextPc 

stepCompare :: Regs r => (MWord -> MWord -> Bool) ->
  r -> r -> Operand r MWord -> InterpM r s Hopefully ()
stepCompare flag rd r1 op2 = do
  x <- regVal r1
  y <- opVal op2
  sMach . mReg rd .= fromIntegral (fromEnum (flag x y))
  nextPc

stepMove :: Regs r => r -> Operand r MWord -> Operand r MWord -> InterpM r s Hopefully ()
stepMove rd cond op2 = do
  y <- opVal op2
  ok <- (/= 0) <$> (opVal cond)
  when ok $ sMach . mReg rd .= y
  nextPc

stepJump :: Regs r => Operand r MWord -> Bool -> Operand r MWord -> InterpM r s Hopefully ()
stepJump cond pos op2 = do
  y <- opVal op2
  cond' <- opVal cond
  if pos `xnor` (0 /= cond') then sMach . mPc .= y else nextPc
    where xnor = (==)
checkPoison :: MWord -> InterpM r s Hopefully ()
checkPoison addr = do
  psn <- use $ sMach . mPsn
  when (addr `member` psn) setBug
  where setBug = sMach . mBug .= True

splitAddr :: MWord -> (MWord, Int)
splitAddr a = (waddr, offset)
  where
    waddr = a `shiftR` logWordBytes
    offset = fromIntegral $ a .&. ((1 `shiftL` logWordBytes) - 1)

splitAlignedAddr :: MemWidth -> MWord -> InterpM r s Hopefully (MWord, Int)
splitAlignedAddr w a = do
  let (waddr, offset) = splitAddr a
  when (offset `mod` widthInt w /= 0) $ do
    cyc <- use $ sMach . mCycle
    otherError $ "unaligned access of " ++ show (widthInt w) ++ " bytes at " ++
        showHex a ++ " on cycle " ++ show cyc
  return (waddr, offset)

subBytes :: Functor f => MemWidth -> Int -> (MWord -> f MWord) -> MWord -> f MWord
subBytes w off f x = put <$> f x'
  where
    offBits = off * 8
    mask = (1 `shiftL` (widthInt w * 8)) - 1
    -- Extract `w` bytes from `x` at `off`
    x' = (x `shiftR` offBits) .&. mask
    -- Insert `w` bytes of `y'` into `x` at `off`
    put y' = (x .&. complement (mask `shiftL` offBits)) .|. ((y' .&. mask) `shiftL` offBits)

stepStore :: Regs r => MemWidth -> Operand r MWord -> r -> InterpM r s Hopefully ()
stepStore w op2 r1 = do
  (waddr, offset) <- splitAlignedAddr w =<< opVal op2
  val <- regVal r1
  checkPoison waddr
  sMach . mMemWord waddr . subBytes w offset .= val
  nextPc

stepLoad :: Regs r => MemWidth -> r -> Operand r MWord -> InterpM r s Hopefully ()
stepLoad w rd op2 = do
  (waddr, offset) <- splitAlignedAddr w =<< opVal op2
  val <- use $ sMach . mMemWord waddr . subBytes w offset
  checkPoison waddr
  sMach . mReg rd .= val
  nextPc

poison :: Regs r => MemWidth -> Operand r MWord -> InterpM r s Hopefully ()
poison w op2 = do
  when (w /= WWord) $ do
    pc <- use $ sMach . mPc
    otherError $ "bad poison width " ++ show w ++ " at pc = " ++ showHex pc
  (waddr, _offset) <- splitAlignedAddr w =<< opVal op2
  sMach . mPsn %= (Set.insert waddr)
  -- don't modify pc. This is not a full step!

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

finishInstr :: InterpM r s Hopefully ()
finishInstr = do
  sMach . mPc %= (+ 1)
  sMach . mCycle %= (+ 1)


toSignedInteger :: MWord -> Integer
toSignedInteger x = toInteger x - if msb x then 1 `shiftL` wordBits else 0

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
  val <- regVal r1
  recordAdvice advice (MemOp addr val MOStore w)
adviceHandler advice (Iload w _rd op2) = do
  addr <- opVal op2
  (waddr, offset) <- splitAlignedAddr w addr
  val <- use $ sMach . mMemWord waddr . subBytes w offset
  recordAdvice advice (MemOp addr val MOLoad w)
adviceHandler advice (Ipoison w op2 r1) = do
  addr <- opVal op2
  val <- regVal r1
  recordAdvice advice (MemOp addr val MOPoison w)
adviceHandler _ _ = return ()


-- Trace handler (for debugging)

traceHandler :: Regs r => Bool -> InstrHandler r s -> InstrHandler r s
traceHandler active _nextH (Iext "trace" ops) = do
  vals <- mapM opVal ops
  when active $ traceM $ "TRACE " ++ intercalate ", " (map show vals)
  nextPc
traceHandler active _nextH (Iext name ops) | Just desc <- Text.stripPrefix "trace_" name = do
  vals <- mapM opVal ops
  when active $ traceM $ "TRACE[" ++ Text.unpack desc ++ "] " ++ intercalate ", " (map show vals)
  nextPc
traceHandler _active nextH instr = nextH instr


-- Memory allocation tracking

data Alloc = Alloc
  { _aLen :: MWord
  , _aFreed :: Bool
  }

data MemErrorKind = OutOfBounds | UseAfterFree | Unallocated
  deriving (Show, Eq)

data AllocState = AllocState
  -- | A map for tracking all allocations made by the program, including those
  -- that have already been freed.  This works becaus eall allocations are
  -- nonoverlapping and memory is never reused.  The map key is the
  -- allocation's start address; use `Map.lookupLE addr` to find the allocation
  -- that might contain `addr`.
  { _asAllocs :: Map MWord Alloc
  -- | The next unused address.  Used for future allocations.
  , _asFrontier :: MWord
  -- | The address of each `malloc` performed by the program.  This is passed
  -- to the second run so it can replicate the same `malloc` behavior, without
  -- having to repeat the same allocation logic.  (Currently, the logic is
  -- simple - just a bump pointer - but it will become a bit more complex in
  -- the future to handle "use before alloc" errors, initial memory, and more
  -- efficient usage of the address space.)
  , _asMallocAddrs :: Seq MWord
  , _asMemErrors :: Seq (MemErrorKind, MWord)
  }

makeLenses ''Alloc
makeLenses ''AllocState

_useALen :: Lens' Alloc MWord
_useALen = aLen

heapStart :: MWord
heapStart = 0x10000000

initAllocState :: AllocState
initAllocState = AllocState Map.empty heapStart Seq.empty Seq.empty

redzoneSize :: MWord
redzoneSize = 128

ceilLog2 :: MWord -> Int
ceilLog2 0 = 1
ceilLog2 n = wordBits - countLeadingZeros (n - 1)


-- | Compute the bounds of the usable space in the block containing `addr`.
blockBounds :: MWord -> (MWord, MWord)
blockBounds addr = (start, end)
  where
    sizeClass = addr `shiftR` 58
    size = 1 `shiftL` fromIntegral sizeClass
    start = addr .&. complement (size - 1)
    -- The last word of the block is reserved for metadata.
    end = start + size - 1

-- | Check the allocation status of `addr`, and record a memory error if it
-- isn't valid.
checkAccess :: Regs r => Bool -> Lens' s AllocState -> MWord -> InterpM r s Hopefully ()
checkAccess _ _ addr | addr < heapStart = return ()
checkAccess verbose allocState addr = do
  let (start, end) = blockBounds addr
  optAlloc <- use $ sExt . allocState . asAllocs . at start
  case optAlloc of
    -- We check against `end` to avoid complaining about accesses of the
    -- block's metadata word.
    Just (Alloc len _) | start + len <= addr && addr < end -> do
      when verbose $ traceM $ "detected out-of-bounds access at " ++ showHex addr ++
        ", after the block at " ++ showHex start
      sExt . allocState . asMemErrors %= (Seq.|> (OutOfBounds, addr))
    Just (Alloc _ True) | start <= addr && addr < end -> do
      when verbose $ traceM $ "detected use-after-free at " ++ showHex addr ++
        ", in the block at " ++ showHex start
      sExt . allocState . asMemErrors  %= (Seq.|> (UseAfterFree, addr))
    Nothing -> do
      when verbose $ traceM $ "detected bad access at " ++ showHex addr ++
        ", in the unallocated block at " ++ showHex start
      sExt . allocState . asMemErrors  %= (Seq.|> (Unallocated, addr))
    _ -> return ()

allocHandler :: Regs r => Bool -> Lens' s AllocState -> InstrHandler r s -> InstrHandler r s
allocHandler verbose allocState _nextH (Iextval "malloc" rd [sizeOp]) = do
  size <- opVal sizeOp
  let sizeClass = ceilLog2 (size + fromIntegral wordBytes)
  let size' = 1 `shiftL` sizeClass

  base <- use $ sExt . allocState . asFrontier
  -- Start at `base + redzoneSize`, then round up to the next multiple of size'
  let addr = (base + redzoneSize + size' - 1) .&. complement (size' - 1)
  sExt . allocState . asFrontier .= addr + size'

  let ptr = addr .|. (fromIntegral sizeClass `shiftL` 58)
  sExt . allocState . asAllocs %= Map.insert ptr (Alloc size False)
  sExt . allocState . asMallocAddrs %= (Seq.|> ptr)

  when verbose $ traceM $ "malloc " ++ show size ++ " bytes (extended to " ++ show size' ++
    ") at " ++ showHex ptr

  sMach . mReg rd .= ptr
  finishInstr
allocHandler verbose allocState _nextH (Iext "free" [ptrOp]) = do
  ptr <- opVal ptrOp

  let sizeClass = ptr `shiftR` 58
  let size' = (1 :: MWord) `shiftL` fromIntegral sizeClass
  when verbose $ traceM $ "free " ++ show size' ++ " words at " ++ showHex ptr

  cur <- use $ sExt . allocState . asAllocs . at ptr
  case cur of
    Just (Alloc _ True) -> do
      when verbose $ traceM $ "detected double free at " ++ showHex ptr
      -- This kind of bug is detected automatically - no advice required.
    Nothing -> do
      when verbose $ traceM $ "detected invalid free at " ++ showHex ptr
      -- This kind of bug is detected automatically - no advice required.
    _ -> return ()

  sExt . allocState . asAllocs . ix ptr . aFreed .= True
  finishInstr
allocHandler _verbose _allocState _nextH (Iextval "advise_poison" rd [_lo, _hi]) = do
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
memErrorHandler info advice _nextH (Iextadvise "malloc" rd [_size]) = do
  val <- maybe (assumptError "ran out of malloc addrs") return =<<
    use (sExt . info . miMallocAddrs . to (Seq.lookup 0))
  sExt . info . miMallocAddrs %= Seq.drop 1
  doAdvise advice rd val
  finishInstr
memErrorHandler info advice _nextH (Iextadvise "advise_poison" rd [loOp, hiOp]) = do
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
      replicateM_ (fromIntegral steps) goStep
      get

    goStep = do
      pc <- use $ sMach . mPc
      i <- fetchInstr pc
      handler i

runPass1 :: Regs r => Bool -> Word -> Sparsity -> MachineState r -> Hopefully MemInfo
runPass1 verbose steps spars initMach' = do
  final <- runWith handler steps initState
  return $ getMemInfo $ final ^. sExt
  where
    initState = InterpState initAllocState initMach' (initSparsSt spars)
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

runPass2 :: Regs r => Word -> Sparsity -> MachineState r -> MemInfo -> Hopefully (Trace r)
runPass2 steps spars initMach' memInfo = do
  -- The first entry of the trace is always the initial state.  Then `steps`
  -- entries follow after it.
  initExecState <- evalStateT (getStateWithAdvice eAdvice) initState
  final <- runWith handler steps initState
  return $ initExecState : toList (final ^. sExt . eTrace)
  where
    initState = InterpState (Seq.empty, Map.empty, memInfo) initMach' (initSparsSt spars)

    eTrace :: Lens' (a, b, c) a
    eTrace = _1
    eAdvice :: Lens' (a, b, c) b
    eAdvice = _2
    eMemInfo :: Lens' (a, b, c) c
    eMemInfo = _3

    handler =
      execTraceHandler eTrace eAdvice $
      controlSparsity eAdvice $
      observer (adviceHandler eAdvice) $
      memErrorHandler eMemInfo eAdvice $
      traceHandler False $
      stepInstr



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
  -- | Locations that have been poisoned
  , psn :: Poison
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
  bug <- use $ sMach . mBug
  psn <- use $ sMach . mPsn
  let inv = False
  answer <- use $ sMach . mAnswer
  return $ ExecutionState pc regs mem psn adv flag bug inv (maybe 0 id answer)

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
  , _mPsn = Set.empty
  , _mBug = False
  , _mAnswer = Nothing
  }

type Executor mreg r = CompilationResult (Prog mreg) -> r
-- | Produce the trace of a program
run_v :: Regs mreg => Bool -> Executor mreg (Trace mreg)
run_v verbose (CompUnit progs trLen _ analysis initMem _) = case go of
  Left e -> error $ describeError e
  Right x -> x
  where
    go = do
      memInfo <- runPass1 verbose  (trLen - 1) sparsity (initMach (highProg progs) initMem)
      tr <- runPass2 (trLen - 1) sparsity (initMach (lowProg progs) initMem) memInfo
      return tr
    sparsity = getSparsity analysis
      
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



