{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RiscV.Simulator where

import qualified Control.Monad.State as MS
import qualified Data.BitVector.Sized as BV
import           Data.BitVector.Sized (BV)
import qualified Data.BitVector.Sized.Unsigned as BVU
import           Data.BitVector.Sized.Unsigned (UnsignedBV(..))
import           Data.Foldable (foldl')
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Type.Equality
import qualified Data.Vector as Vec
import           Data.Vector (Vector)
import           Numeric.Natural

import Data.Parameterized.Classes
import Data.Parameterized.List
import Data.Parameterized.NatRepr
import Data.Parameterized.Some

import GRIFT.BitVector.BVApp (BVApp(..))
import GRIFT.BitVector.BVFloatApp (BVFloatApp(..))
import GRIFT.Decode (encode)
import GRIFT.InstructionSet
import GRIFT.InstructionSet.Known
import GRIFT.Types
import GRIFT.Semantics hiding (concat)
import GRIFT.Semantics.Expand
import GRIFT.Semantics.Pretty
import GRIFT.Semantics.Utils
import GRIFT.Simulation

import Compiler.Errors
import Native
import RiscV.RiscVAsm

data RiscV

data Machine = Machine
  { mRV :: RVRepr RV64IM
  , mPC :: UnsignedBV (RVWidth RV64IM)
  , mMemory :: Map (UnsignedBV (RVWidth RV64IM)) (UnsignedBV 8)
  , mGPRs :: Vector (UnsignedBV (RVWidth RV64IM))
  }

newtype SimM a = SimM { unSimM :: MS.State Machine a }
  deriving (Functor, Applicative, Monad, MS.MonadState Machine)

asInt :: UnsignedBV w -> Int
asInt = fromInteger . BV.asUnsigned . BVU.asBV

-- | Concatenate a list of 'UnsignedBV's into an 'UnsignedBV' of arbitrary width. The ordering is little endian:
--
-- >>> bvConcatMany' [0xAA :: UnsignedBV 8, 0xBB] :: UnsignedBV 16
-- 0xbbaa
-- >>> bvConcatMany' [0xAA :: UnsignedBV 8, 0xBB, 0xCC] :: UnsignedBV 16
-- 0xbbaa
--
-- If the sum of the widths of the input 'UnsignedBV's exceeds the output width, we
-- ignore the tail end of the list.
bvConcatMany' :: forall w. NatRepr w -> [UnsignedBV 8] -> UnsignedBV w
bvConcatMany' repr uBvs = UnsignedBV $ foldl' go (zero repr) (zip [0..] uBvs)
  where
    go :: BV w -> (Natural, UnsignedBV 8) -> BV w
    go acc (i, UnsignedBV bv) =
      let bvExt = BV.zresize repr bv in
      let bvShifted = BV.shl repr bvExt (i * 8) in
      BV.or acc bvShifted

-- | Given a 'BV' of arbitrary length, decompose it into a list of bytes. Uses
-- an unsigned interpretation of the input vector, so if you ask for more bytes that
-- the 'BV' contains, you get zeros. The result is little-endian, so the first
-- element of the list will be the least significant byte of the input vector.
bvGetBytesU :: Int -> BV w -> [BV 8]
bvGetBytesU n _ | n <= 0 = []
bvGetBytesU n bv = map go [0..(n-1)]
  where
    go i = select' ((fromIntegral i) * 8) (knownNat @8) bv

instance RVStateM SimM RV64IM where
  getRV = return rv64IMRepr
  getPC = MS.gets mPC
  getGPR rid = (Vec.! asInt rid) <$> MS.gets mGPRs
  getFPR = undefined
  getMem bytes addr = do
    memory <- MS.gets mMemory
    let val = fmap (\a -> Map.findWithDefault 0 a memory)
              [addr..addr+(fromIntegral (natValue bytes-1))]
    return (bvConcatMany' ((knownNat @8) `natMultiply` bytes) val)
  getCSR = undefined
  getPriv = undefined

  setPC pc = MS.modify $ \m -> m { mPC = pc }
  setGPR rid regVal = MS.modify $ \m ->
    let gprs = mGPRs m
    in m { mGPRs = Vec.update gprs [(asInt rid, regVal)] }
  setFPR = undefined
  setMem bytes addr (UnsignedBV val) = do
    memory <- MS.gets mMemory
    let addrValPairs = zip
          [addr + (fromIntegral i) | i <- [0..(natValue bytes-1)]]
          (UnsignedBV <$> bvGetBytesU (fromIntegral (natValue bytes)) val)
        memory' = foldr (\(a, byte) mem -> Map.insert a byte mem) memory addrValPairs
    MS.modify $ \m -> m { mMemory = memory' }
  setCSR = undefined
  setPriv = undefined
  isHalted = undefined
  logInstruction = undefined

stepInstM :: Instruction RV64IM fmt -> SimM ()
stepInstM i@(Inst opcode _) = do
  let semantics = getInstSemantics $ semanticsFromOpcode knownISet opcode
      iw = 32
  execSemantics (evalInstExpr knownISet i iw) semantics

stepInst :: Machine -> Instruction RV64IM fmt -> Machine
stepInst m i = MS.execState (unSimM (stepInstM i)) m

instance TestEquality Operands where
  Operands fmt1 ops1 `testEquality` Operands fmt2 ops2
    | Just Refl <- fmt1 `testEquality` fmt2
    , Just Refl <- ops1 `testEquality` ops2 = Just Refl
    | otherwise = Nothing

instance OrdF Operands where
  Operands fmt1 ops1 `compareF` Operands fmt2 ops2 =
    case fmt1 `compareF` fmt2 of
      EQF -> fromOrdering (toOrdering (ops1 `compareF` ops2))
      c -> c

instance TestEquality (Instruction RV64IM) where
  Inst oc1 ops1 `testEquality` Inst oc2 ops2
    | Just Refl <- oc1 `testEquality` oc2
    , Just Refl <- ops1 `testEquality` ops2 = Just Refl
    | otherwise = Nothing

instance OrdF (Instruction RV64IM) where
  Inst oc1 ops1 `compareF` Inst oc2 ops2 =
    case oc1 `compareF` oc2 of
      EQF -> ops1 `compareF` ops2
      c -> c

instance Native RiscV where
  type Inst RiscV = Some (Instruction RV64IM)
  type State RiscV = Machine

  toMRAMInsts i = undefined
  stepArch m (Some i) = Right (stepInst m i)
  toArchState ms' = undefined
  archStateEq s1 s2 = undefined

sizedBVToReg :: SizedBV 5 -> Reg
sizedBVToReg _ = undefined

griftToInstr :: Instruction RV64IM fmt -> Instr
griftToInstr (Inst oc (Operands fmt ops)) =
  case fmt of
    RRepr | (rdBV :< rs1BV :< rs2BV :< Nil) <- ops ->
            let rd = sizedBVToReg rdBV
                rs1 = sizedBVToReg rs1BV
                rs2 = sizedBVToReg rs2BV
            in case oc of
              Add -> Instr32I $ RegBinop32 ADD rd rs1 rs2
              Sub -> undefined
              Sll -> undefined
              Slt -> undefined
              Sltu -> undefined
              Xor -> undefined
              Srl -> undefined
              Sra -> undefined
              Or -> undefined
              And -> undefined
              Addw -> undefined
              Subw -> undefined
              Sllw -> undefined
              Srlw -> undefined
              Sraw -> undefined
              Slliw -> undefined
              Srliw -> undefined
              Sraiw -> undefined
              Mul -> undefined
              Mulh -> undefined
              Mulhsu -> undefined
              Mulhu -> undefined
              Div -> undefined
              Divu -> undefined
              Rem -> undefined
              Remu -> undefined
              Mulw -> undefined
              Divw -> undefined
              Divuw -> undefined
              Remw -> undefined
              Remuw -> undefined
    IRepr -> undefined
    SRepr -> undefined
    BRepr -> undefined
    URepr -> undefined
    JRepr -> undefined
    HRepr -> undefined
    PRepr -> undefined
    ARepr -> undefined
    R2Repr -> undefined
    R3Repr -> undefined
    R4Repr -> undefined
    RXRepr -> undefined
    XRepr -> undefined
