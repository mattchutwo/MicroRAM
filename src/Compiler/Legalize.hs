{-|
Module      : Legalize
Description : Simplifies instructions where the first operand is not a register. 
Maintainer  : santiago@galois.com
Stability   : prototype

This module compiles MicroIR to RTL.  It looks for instruction formats that
aren't legal in RTL/MicroRAM, such as addition of two constants, and replaces
them with legal sequences.  Usually this means adding an `Imov` to put one
operand into a register, but for some instructions we can do better.
-}

module Compiler.Legalize
    ( legalize,
    ) where

import Control.Monad
import Control.Monad.State


import Compiler.Errors
import Compiler.IRs

import MicroRAM (Instruction'(..))
import           MicroRAM (MWord)
import qualified MicroRAM as MRAM


type Statefully m w a = StateT (LegalizeState m w) Hopefully a

data LegalizeState m w = LegalizeState
  { lsNextReg :: Word
  -- | Metadata of the instruction being legalized.  Any additional
  -- instructions will get a copy of the same metadata.
  , lsMetadata :: Maybe m
  -- | Additional instructions to insert before the instruction being
  -- legalized.  For example, we may need to insert an `Imov` to store a
  -- non-register operand into a register.  This list is kept in reverse
  -- order to make `emitInstr` more efficient.
  , lsPending :: [RTLInstr m w]
  }

-- | Run a `Statefully` action, returning the result and the new value of
-- `nextReg`.
runStatefully :: Statefully m w a -> Word -> Hopefully (a, Word)
runStatefully act nextReg = do
  (result, s) <- runStateT act $ LegalizeState
    { lsNextReg = nextReg
    , lsMetadata = Nothing
    , lsPending = []
    }
  when (not $ null $ lsPending s) $
    assumptError "internal error: failed to flush pending instructions"
  return (result, lsNextReg s)


getMetadata :: Statefully m w m
getMetadata = gets lsMetadata >>= \x -> case x of
  Just x -> return x
  Nothing -> assumptError "tried to access metadata with no current instruction"

withMetadata :: m -> Statefully m w a -> Statefully m w a
withMetadata m act = do
  -- TODO: lens
  oldM <- gets lsMetadata
  modify $ \s -> s { lsMetadata = Just m }
  result <- act
  modify $ \s -> s { lsMetadata = oldM }
  return result

freshReg :: Statefully m w Name
freshReg = do
  x <- state $ \s -> (lsNextReg s, s { lsNextReg = lsNextReg s + 1 })
  return $ NewName x

emitInstr :: RTLInstr m w -> Statefully m w ()
emitInstr i = do
  -- TODO: lens
  modify $ \s -> s { lsPending = i : lsPending s}

-- | Emit an instruction, using the current metadata.
emitInstr' :: (m -> RTLInstr m w) -> Statefully m w ()
emitInstr' i = do
  m <- getMetadata
  emitInstr (i m)

flushPending :: Statefully m w [RTLInstr m w]
flushPending = state $ \s -> (reverse $ lsPending s, s { lsPending = [] })


op2reg :: MAOperand VReg w -> Statefully m w VReg
op2reg (AReg r) = return r
op2reg o = do
  r <- freshReg
  emitInstr' $ MRI (MRAM.Imov r o)
  return r

-- | Legalize a single instruction.
legalizeInstr :: MIRInstr m w -> Statefully m w [RTLInstr m w]
legalizeInstr i = withMetadata m $ do
  i' <- legalizeInstr' i
  -- Add `i'` to the pending instructions list, so it's included in the output
  -- of `flushPending`.
  emitInstr i'
  flushPending
  where m = case i of
          MirI _ m -> m
          MirM _ m -> m

-- | Core logic of `legalizeInstr`.  This legalizes a single instruction,
-- possibly adding other instructions with `emitInstr`.
legalizeInstr' :: MIRInstr m w -> Statefully m w (RTLInstr m w)
legalizeInstr' (MirI i m) = return $ IRI i m
legalizeInstr' (MirM i m) = do
  i' <- case i of
    Iand rd o1 (AReg r2) -> return $ Iand rd r2 o1
    Iand rd o1 o2 -> Iand rd <$> op2reg o1 <*> pure o2
    Ior rd o1 (AReg r2) -> return $ Ior rd r2 o1
    Ior rd o1 o2 -> Ior rd <$> op2reg o1 <*> pure o2
    Ixor rd o1 (AReg r2) -> return $ Ixor rd r2 o1
    Ixor rd o1 o2 -> Ixor rd <$> op2reg o1 <*> pure o2
    Inot rd o2 -> return $ Inot rd o2

    Iadd rd o1 (AReg r2) -> return $ Iadd rd r2 o1
    Iadd rd o1 o2 -> Iadd rd <$> op2reg o1 <*> pure o2
    Isub rd o1 o2 -> Isub rd <$> op2reg o1 <*> pure o2
    Imull rd o1 (AReg r2) -> return $ Imull rd r2 o1
    Imull rd o1 o2 -> Imull rd <$> op2reg o1 <*> pure o2
    Iumulh rd o1 (AReg r2) -> return $ Iumulh rd r2 o1
    Iumulh rd o1 o2 -> Iumulh rd <$> op2reg o1 <*> pure o2
    Ismulh rd o1 (AReg r2) -> return $ Ismulh rd r2 o1
    Ismulh rd o1 o2 -> Ismulh rd <$> op2reg o1 <*> pure o2
    Iudiv rd o1 o2 -> Iudiv rd <$> op2reg o1 <*> pure o2
    Iumod rd o1 o2 -> Iumod rd <$> op2reg o1 <*> pure o2

    Ishl rd o1 o2 -> Ishl rd <$> op2reg o1 <*> pure o2
    Ishr rd o1 o2 -> Ishr rd <$> op2reg o1 <*> pure o2

    Icmpe o1 (AReg r2) -> return $ Icmpe r2 o1
    Icmpe o1 o2 -> Icmpe <$> op2reg o1 <*> pure o2
    Icmpa o1 o2 -> Icmpa <$> op2reg o1 <*> pure o2
    Icmpae o1 o2 -> Icmpae <$> op2reg o1 <*> pure o2
    Icmpg o1 o2 -> Icmpg <$> op2reg o1 <*> pure o2
    Icmpge o1 o2 -> Icmpge <$> op2reg o1 <*> pure o2

    Imov rd o2 -> return $ Imov rd o2
    Icmov rd o2 -> return $ Icmov rd o2

    Ijmp o2 -> return $ Ijmp o2
    Icjmp o2 -> return $ Icjmp o2
    Icnjmp o2 -> return $ Icnjmp o2

    Istore o2 o1 -> Istore <$> pure o2 <*> op2reg o1
    Iload rd o2 -> return $ Iload rd o2

    Iread rd o2 -> return $ Iread rd o2
    Ianswer o2 -> return $ Ianswer o2

    Ipoison o2 o1 -> Ipoison <$> pure o2 <*> op2reg o1
    Iadvise rd -> return $ Iadvise rd

    Iext name ops -> return $ Iext name ops
    Iextval name rd ops -> return $ Iextval name rd ops
    Iextadvise name rd ops -> return $ Iextadvise name rd ops
  return $ MRI i' m

legalizeInstrs :: [MIRInstr m w] -> Statefully m w [RTLInstr m w]
legalizeInstrs instrs = concat <$> mapM legalizeInstr instrs

legalizeBlock :: BB Name (MIRInstr m w) -> Statefully m w (BB Name (RTLInstr m w))
legalizeBlock (BB name body term dag) =
  BB name <$> legalizeInstrs body <*> legalizeInstrs term <*> pure dag

legalizeFunc :: MIRFunction () MWord -> Hopefully (RFunction () MWord)
legalizeFunc f = do
  (blocks', nextReg') <- runStatefully (mapM legalizeBlock $ funcBlocks f) (funcNextReg f)
  return $ f { funcBlocks = blocks', funcNextReg = nextReg' }

legalize :: MIRprog () MWord -> Hopefully (Rprog () MWord)
legalize p = do
  code' <- mapM legalizeFunc $ code p
  return $ p { code = code' }
