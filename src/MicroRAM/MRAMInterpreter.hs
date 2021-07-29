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
    -- For post processing check (e.g. segment checking)
    runWith, initMach, InstrHandler, runPassGeneric, InterpState, sMach, sExt, mCycle,
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

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.State
import Control.Lens (makeLenses, ix, at, to, lens, (^.), (^?), (&), (.~), (.=), (%=), use, Lens', _1, _2, _3)
import Data.Bits
import qualified Data.ByteString as BS
import Data.Foldable
import Data.List (intercalate)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Set (Set, member)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Word

import GHC.Generics (Generic)


import Compiler.Errors
import Compiler.Registers
import Compiler.Tainted
import Compiler.CompilationUnit
import MicroRAM

import Util.Util

import Debug.Trace

type Poison = Set MWord
data Mem = Mem
  MWord
  (Map MWord MWord)
  (Maybe (Map MWord (Vector Label))) -- ^ Vector of 8 (2 bit) labels for the word.


data MachineState r = MachineState
  { _mCycle :: MWord
  , _mPc :: MWord
  , _mRegs :: RegBank r MWord
  , _mRegLabels :: Maybe (RegBank r (Vector Label)) -- Currently, when the `--mode leak-tainted` flag is provided, the tainted labels will be present as a `Just`.
                                                    -- When the flag is not provided, this will be `Nothing`.
                                                    -- In the future, instead of dynamically tracking whether the flag is provided, we could use the type system to ensure labels are tracked only when the tainted flag is provided.
                                                    -- Similar to `IfMode` in the witness checker, we could change this definition to something like `IfMode tainted (RegBank r (Vector Label))`.
  , _mProg :: Seq (Instruction r MWord)
  , _mMem :: Mem
  , _mPsn :: Poison
  , _mBug :: Bool
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

checkLeakTainted :: Monad m => InterpM r s m Bool
checkLeakTainted = do
  ls <- use $ sMach . mRegLabels
  return $ isJust ls

-- | Lens for accessing a particular register in `mRegs`.  Returns the default
-- value 0 when the register is uninitialized.
mReg :: (Functor f, Regs r) => r -> (MWord -> f MWord) -> (MachineState r -> f (MachineState r))
mReg r = mRegs . lens (maybe 0 id . lookupReg r) (flip $ updateBank r)

mRegLabel :: (Functor f, Regs r) => r -> (Maybe (Vector Label) -> f (Maybe (Vector Label))) -> (MachineState r -> f (MachineState r))
mRegLabel r = mRegLabels . lens (fmap (maybe untaintedWord id . lookupReg r)) (liftA2 (flip $ updateBank r))

-- | Lens for accessing a particular word of memory.  Produces the `Mem`'s
-- default value when reading an uninitialized location.
memWord :: Functor f => MWord -> (MWord -> f MWord) -> (Mem -> f Mem)
memWord addr = lens (get addr) (set addr)
  where
    get addr (Mem d m _l) = maybe d id $ Map.lookup addr m
    set addr (Mem d m l) val = Mem d (Map.insert addr val m) l

mMemWord :: Functor f => MWord -> (MWord -> f MWord) -> (MachineState r -> f (MachineState r))
mMemWord addr = mMem . memWord addr

mMemLabel :: Functor f => MWord -> (Maybe (Vector Label) -> f (Maybe (Vector Label))) -> (MachineState r -> f (MachineState r))
mMemLabel addr = mMem . memLabel addr
  where
    -- | Lens for accessing a particular label of memory.  Produces the
    -- untainted when reading an uninitialized location.
    memLabel :: Functor f => MWord -> (Maybe (Vector Label) -> f (Maybe (Vector Label))) -> (Mem -> f Mem)
    memLabel addr = lens (get addr) (set addr)

    get addr (Mem _d _m l) = (maybe untaintedWord id . Map.lookup addr) <$> l
    set addr (Mem d m l) val = Mem d m (Map.insert addr <$> val <*> l)


-- Retrieves the subvector with the given width and at the given offset. 
-- Updates the vector at the given offset with the first width entries of the new vector.
subLabels :: Functor f => MemWidth -> Int -> (Maybe (Vector Label) -> f (Maybe (Vector Label))) -> Maybe (Vector Label) -> f (Maybe (Vector Label))
subLabels wd offset f ls = put <$> f ls'
  where
    w = widthInt wd
    ls' = Vec.slice offset w <$> ls
    -- Assumes that the provided vector has a width of at least w.
    put v' = do
      (ls1, lsr) <- Vec.splitAt offset <$> ls
      let (_ls2, ls3) = Vec.splitAt w lsr
      v <- (Vec.take w) <$> v'
      return $ Vec.concat [ls1, v, ls3]

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
  stepInstrTainted i

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

    Isink _w _rj _op2 -> nextPc
    Itaint _w _rj _op2 -> nextPc

    Iadvise _ -> assumptError $ "unhandled advice request"

    i -> do
      -- Replace input operands with their values, and convert the destination
      -- register to a `Word` so it can be printed.
      i' <- mapInstrM (return . toWord) regVal opVal i
      assumptError $ "unhandled instruction: " ++ show i'

  sMach . mCycle %= (+ 1)

-- TODO: Move to Compiler.Tainted?
stepInstrTainted :: Regs r => Instruction r MWord -> InterpM r s Hopefully ()
stepInstrTainted (Imov rd op2) = do
  l <- opLabel op2
  sMach . mRegLabel rd .= l
stepInstrTainted (Icmov rd cond op2) = do
  ok <- (/= 0) <$> (regVal cond)
  when ok $ do
    l <- opLabel op2
    sMach . mRegLabel rd .= l

stepInstrTainted (Isink wd rj op2) = do
  -- Write of value in `rj` to label `op2`.
  -- Bug here if label of rj cannot flow into op2.
  ljsM <- fmap (Vec.take (widthInt wd)) <$> regLabel rj
  checkLabels ljsM
  l2 <- opVal op2 >>= toLabel
  let isBug = fromMaybe False $ do
        ljs <- ljsM
        return $ any (\lj -> not $ lj `canFlowTo` l2) ljs
  when isBug $
    sMach . mBug .= True
stepInstrTainted (Itaint wd rj op2) = do
  l <- opVal op2 >>= toLabel -- Taint register rj with label value op2.
  let offset = 0
  sMach . mRegLabel rj . subLabels wd offset .= Just (replicateWord l)

stepInstrTainted (Iload wd rd op2) = do
  addr <- opVal op2

  -- Get word address and offset.
  (waddr, offset) <- splitAlignedAddr wd addr

  -- Get labels for the address and offset.
  ls <- use $ sMach . mMemLabel waddr . subLabels wd offset

  -- Pad the unused labels.
  let l = padUntaintedWord <$> ls

  -- Set the register label.
  sMach . mRegLabel rd .= l
stepInstrTainted (Istore wd op2 r1) = do
  addr <- opVal op2
  (waddr, offset) <- splitAlignedAddr wd addr
  l <- regLabel r1
  checkLabels l
  sMach . mMemLabel waddr . subLabels wd offset .= l
stepInstrTainted (Ipoison wd op2 r1) = do
  -- Poison must be a full word.
  requireWWord wd
  addr <- opVal op2
  (waddr, _offset) <- splitAlignedAddr wd addr
  l <- regLabel r1
  checkLabels l
  sMach . mMemLabel waddr .= l

-- Currently, we untaint the written to register for the following instructions.
stepInstrTainted (Iand rd _r1 _op2) = stepUntaintReg rd
stepInstrTainted (Ior rd _r1 _op2) = stepUntaintReg rd
stepInstrTainted (Ixor rd _r1 _op2) = stepUntaintReg rd
stepInstrTainted (Inot rd _op2) = stepUntaintReg rd

stepInstrTainted (Iadd rd _r1 _op2) = stepUntaintReg rd
stepInstrTainted (Isub rd _r1 _op2) = stepUntaintReg rd
stepInstrTainted (Imull rd _r1 _op2) = stepUntaintReg rd
stepInstrTainted (Iumulh rd _r1 _op2) = stepUntaintReg rd
stepInstrTainted (Ismulh rd _r1 _op2) = stepUntaintReg rd
stepInstrTainted (Iudiv rd _r1 _op2) = stepUntaintReg rd
stepInstrTainted (Iumod rd _r1 _op2) = stepUntaintReg rd

stepInstrTainted (Ishl rd _r1 _op2) = stepUntaintReg rd
stepInstrTainted (Ishr rd _r1 _op2) = stepUntaintReg rd

stepInstrTainted (Icmpe r1 _op1 _op2) = stepUntaintReg r1
stepInstrTainted (Icmpa r1 _op1 _op2) = stepUntaintReg r1
stepInstrTainted (Icmpae r1 _op1 _op2) = stepUntaintReg r1
stepInstrTainted (Icmpg r1 _op1 _op2) = stepUntaintReg r1
stepInstrTainted (Icmpge r1 _op1 _op2) = stepUntaintReg r1

stepInstrTainted (Ijmp _op2) = return ()
stepInstrTainted (Icjmp _op1 _op2) = return ()
stepInstrTainted (Icnjmp _op1 _op2) = return ()

stepInstrTainted (Iread rd _op2) = stepUntaintReg rd
stepInstrTainted (Ianswer _op2) = return ()

stepInstrTainted (Iadvise _) = assumptError $ "unhandled advice request"

stepInstrTainted i@(Iext _) = do
      i' <- mapInstrM (return . toWord) regVal opVal i
      assumptError $ "unhandled extension instruction: " ++ show i'
stepInstrTainted i@(Iextval _ _) = do
      i' <- mapInstrM (return . toWord) regVal opVal i
      assumptError $ "unhandled extension instruction: " ++ show i'
stepInstrTainted i@(Iextadvise _ _) = do
      i' <- mapInstrM (return . toWord) regVal opVal i
      assumptError $ "unhandled extension instruction: " ++ show i'

-- whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
-- whenJust = for_

stepUntaintReg :: Regs r => r -> InterpM r s Hopefully ()
stepUntaintReg rd = do
  check <- checkLeakTainted
  when check $
    sMach . mRegLabel rd .= Just untaintedWord

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

-- | Takes a byte memory address and returns the corresponding a word memory address and byte offset.
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
  addr <- opVal op2
  val <- regVal r1
  doStore w addr val
  nextPc

doStore :: MemWidth -> MWord -> MWord -> InterpM r s Hopefully ()
doStore w addr val = do
  (waddr, offset) <- splitAlignedAddr w addr
  checkPoison waddr
  sMach . mMemWord waddr . subBytes w offset .= val

stepLoad :: Regs r => MemWidth -> r -> Operand r MWord -> InterpM r s Hopefully ()
stepLoad w rd op2 = do
  addr <- opVal op2
  doLoad w rd addr
  nextPc

doLoad :: Regs r => MemWidth -> r -> MWord -> InterpM r s Hopefully ()
doLoad w rd addr = do
  (waddr, offset) <- splitAlignedAddr w addr
  val <- use $ sMach . mMemWord waddr . subBytes w offset
  checkPoison waddr
  sMach . mReg rd .= val

-- Enforces the invariant that poisons require WWord widths.
requireWWord :: MemWidth -> InterpM r s Hopefully ()
requireWWord w = when (w /= WWord) $ do
    pc <- use $ sMach . mPc
    otherError $ "bad poison width " ++ show w ++ " at pc = " ++ showHex pc

poison :: Regs r => MemWidth -> Operand r MWord -> InterpM r s Hopefully ()
poison w op2 = do
  requireWWord w
  (waddr, _offset) <- splitAlignedAddr w =<< opVal op2
  sMach . mPsn %= (Set.insert waddr)
  -- don't modify pc. This is not a full step!

stepRead :: Regs r => r -> Operand r MWord -> InterpM r s Hopefully ()
stepRead rd _op2 = do
  -- All tapes are empty.
  sMach . mReg rd .= 0
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

regLabel :: Regs r => r -> InterpM r s Hopefully (Maybe (Vector Label))
regLabel r = use $ sMach . mRegLabel r

opVal ::  Regs r => Operand r MWord -> InterpM r s Hopefully MWord
opVal (Reg r) = regVal r
opVal (Const w) = return w

-- Returns the label of an operand.
opLabel ::  Regs r => Operand r MWord -> InterpM r s Hopefully (Maybe (Vector Label))
opLabel (Reg r) = regLabel r
opLabel (Const _w) = do
  check <- checkLeakTainted
  if check then
    return $ Just untaintedWord
  else
    return Nothing


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
                             -- In the future, instead of dynamically tracking whether the tainted flag is provided, we could use the type system to ensure labels are tracked only when the tainted flag is provided.
  | Advise MWord
  | Stutter
  deriving (Eq, Read, Show, Generic)


-- | Pretty printer for advice.
renderAdvc :: [Advice] -> String
renderAdvc advs = concat $ map renderAdvc' advs
  where renderAdvc' :: Advice -> String
        renderAdvc' (MemOp addr v MOStore _w l) = "Store: " ++ show addr ++ "->" ++ show v ++ " (" ++ show l ++ ")"
        renderAdvc' (MemOp  addr v MOLoad _w l) = "Load: " ++ show addr ++ "->" ++ show v ++ " (" ++ show l ++ ")"
        renderAdvc' (MemOp addr v MOPoison _w l) = "Poison: " ++ show addr ++ "->" ++ show v ++ " (" ++ show l ++ ")"
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
  (waddr, offset) <- splitAlignedAddr w addr
  storeVal <- regVal r1
  memVal <- use $ sMach . mMemWord waddr
  let memVal' = memVal & subBytes w offset .~ storeVal
  storeTaint <- regLabel r1
  memTaint <- use $ sMach . mMemLabel waddr
  let memTaint' = memTaint & subLabels w offset .~ storeTaint
  recordAdvice advice (MemOp addr memVal' MOStore w memTaint')
adviceHandler advice (Iload w _rd op2) = do
  addr <- opVal op2
  (waddr, _offset) <- splitAlignedAddr w addr
  val <- use $ sMach . mMemWord waddr
  taint <- use $ sMach . mMemLabel waddr
  recordAdvice advice (MemOp addr val MOLoad w taint)
adviceHandler advice (Ipoison w op2 r1) = do
  -- Poison must be a full word.
  requireWWord w
  addr <- opVal op2
  val <- regVal r1
  taint <- regLabel r1
  recordAdvice advice (MemOp addr val MOPoison w taint)
adviceHandler _ _ = return ()


-- Trace handler (for debugging)

readStr :: Monad m => MWord -> InterpM r s m Text
readStr ptr = do
  let (waddr, offset) = splitAddr ptr
  Mem _ mem _ <- use $ sMach . mMem
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
checkAccess :: Regs r => Bool -> Lens' s AllocState -> MWord -> InterpM r s Hopefully ()
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
  doLoad WWord rd addr
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

type Mem' = (MWord, Map MWord MWord, Maybe (Map MWord (Vector Label))) -- In the future, instead of dynamically tracking whether the tainted flag is provided, we could use the type system to ensure labels are tracked only when the tainted flag is provided.

-- | The program state 
data ExecutionState mreg = ExecutionState {
  -- | Program counter
  pc :: MWord
  -- | Register bank
  , regs :: RegBank mreg MWord
  -- | Register label bank
  , regLabels :: Maybe (RegBank mreg (Vector Label)) -- In the future, instead of dynamically tracking whether the tainted flag is provided, we could use the type system to ensure labels are tracked only when the tainted flag is provided.
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

deriving instance (Ord mreg, Read mreg, Read (RegBank mreg MWord), Read (RegBank mreg Label)) => Read (ExecutionState mreg)
deriving instance (Show mreg, Show (RegBank mreg MWord), Show (RegBank mreg Label)) => Show (ExecutionState mreg)

getStateWithAdvice :: Monad m => Lens' s AdviceMap -> InterpM r s m (ExecutionState r)
getStateWithAdvice advice = do
  pc <- use $ sMach . mPc
  regs <- use $ sMach . mRegs
  regLabels <- use $ sMach . mRegLabels
  Mem d m l <- use $ sMach . mMem
  let mem = (d, m, l)
  cycle <- use $ sMach . mCycle
  -- Retrieve advice for the cycle that just finished executing.
  adv <- use $ sExt . advice . ix (cycle - 1)
  bug <- use $ sMach . mBug
  psn <- use $ sMach . mPsn
  let inv = False
  answer <- use $ sMach . mAnswer
  return $ ExecutionState pc regs regLabels mem psn adv bug inv (maybe 0 id answer)

type Prog mreg = Program mreg MWord
type Trace mreg = [ExecutionState mreg]

initMach :: Regs r => Bool -> Program r MWord -> InitialMem -> MachineState r
initMach leakTainted prog imem = MachineState
  { _mCycle = 0
  , _mPc = 0
  , _mRegs = initBank (lengthInitMem imem)
  , _mRegLabels = if leakTainted then Just $ initBank untaintedWord else Nothing
  , _mProg = Seq.fromList prog
  , _mMem = Mem 0 (flatInitMem imem) $ if leakTainted then Just $ flatInitTaintedMem imem else Nothing
  , _mPsn = Set.empty
  , _mBug = False
  , _mAnswer = Nothing
  }

type Executor mreg r = CompilationResult (Prog mreg) -> r
-- | Produce the trace of a program
run_v :: Regs mreg => Bool -> Bool -> Executor mreg (Trace mreg)
run_v verbose leakTainted (CompUnit progs trLen _ _ _analysis _) = case go of
  Left e -> error $ describeError e
  Right x -> x
  where
    go = do
      let highState = initMach leakTainted (pmProg $ highProg progs) (pmMem $ highProg progs)
      memInfo <- runPass1 verbose  (trLen - 1) highState
      let lowState = initMach leakTainted (pmProg $ lowProg progs) (pmMem $ lowProg progs)
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
load ::  MWord -> Mem' -> MWord
load x (d,m,_l)=  case Map.lookup x m of
                 Just y -> y
                 Nothing -> d



