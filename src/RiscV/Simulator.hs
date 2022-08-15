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

sizedBVToImm :: SizedBV w -> Imm
sizedBVToImm _ = undefined

griftToInstr :: Instruction RV64IM fmt -> Instr
griftToInstr (Inst oc (Operands fmt ops)) =
  case fmt of
    RRepr | (rdBV :< rs1BV :< rs2BV :< Nil) <- ops ->
            let rd = sizedBVToReg rdBV
                rs1 = sizedBVToReg rs1BV
                rs2 = sizedBVToReg rs2BV
                imm = sizedBVToImm rs2BV
            in case oc of
              Add -> Instr32I $ RegBinop32 ADD rd rs1 rs2
              Sub -> Instr32I $ RegBinop32 SUB rd rs1 rs2
              Sll -> Instr32I $ RegBinop32 SLL rd rs1 rs2
              Slt -> Instr32I $ RegBinop32 SLT rd rs1 rs2
              Sltu -> Instr32I $ RegBinop32 SLTU rd rs1 rs2
              Xor -> Instr32I $ RegBinop32 XOR rd rs1 rs2
              Srl -> Instr32I $ RegBinop32 SRL rd rs1 rs2
              Sra -> Instr32I $ RegBinop32 SRA rd rs1 rs2
              Or -> Instr32I $ RegBinop32 OR rd rs1 rs2
              And -> Instr32I $ RegBinop32 AND rd rs1 rs2
              Addw -> Instr64I $ RegBinop64 ADDW rd rs1 rs2
              Subw -> Instr64I $ RegBinop64 SUBW rd rs1 rs2
              Sllw -> Instr64I $ RegBinop64 SLLW rd rs1 rs2
              Srlw -> Instr64I $ RegBinop64 SRLW rd rs1 rs2
              Sraw -> Instr64I $ RegBinop64 SRAW rd rs1 rs2
              Slliw -> Instr64I $ ImmBinop64 SLLIW rd rs1 imm
              Srliw -> Instr64I $ ImmBinop64 SRLIW rd rs1 imm
              Sraiw -> Instr64I $ ImmBinop64 SRAIW rd rs1 imm
              Mul -> Instr32M $ MUL rd rs1 rs2
              Mulh -> Instr32M $ MULH rd rs1 rs2
              Mulhsu -> Instr32M $ MULHSU rd rs1 rs2
              Mulhu -> Instr32M $ MULHU rd rs1 rs2
              Div -> Instr32M $ DIV rd rs1 rs2
              Divu -> Instr32M $ DIVU rd rs1 rs2
              Rem -> Instr32M $ REM rd rs1 rs2
              Remu -> Instr32M $ REMU rd rs1 rs2
              Mulw -> Instr64M $ MULW rd rs1 rs2
              Divw -> Instr64M $ DIVW rd rs1 rs2
              Divuw -> Instr64M $ DIVUW rd rs1 rs2
              Remw -> Instr64M $ REMW rd rs1 rs2
              Remuw -> Instr64M $ REMUW rd rs1 rs2
    IRepr | (rdBV :< rs1BV :< imm12BV :< Nil) <- ops ->
            let rd = sizedBVToReg rdBV
                rs1 = sizedBVToReg rs1BV
                imm12 = sizedBVToImm imm12BV
            in case oc of
              Jalr -> Instr32I $ JALR rd rs1 imm12
              Lb -> Instr32I $ MemInstr32 LB rd imm12 rs1
              Lh -> Instr32I $ MemInstr32 LH rd imm12 rs1
              Lw -> Instr32I $ MemInstr32 LW rd imm12 rs1
              Lbu -> Instr32I $ MemInstr32 LBU rd imm12 rs1
              Lhu -> Instr32I $ MemInstr32 LHU rd imm12 rs1
              Addi -> Instr32I $ ImmBinop32 ADDI rd rs1 imm12
              Slti -> Instr32I $ ImmBinop32 SLTI rd rs1 imm12
              Sltiu -> Instr32I $ ImmBinop32 SLTIU rd rs1 imm12
              Xori -> Instr32I $ ImmBinop32 XORI rd rs1 imm12
              Ori -> Instr32I $ ImmBinop32 ORI rd rs1 imm12
              Andi -> Instr32I $ ImmBinop32 ANDI rd rs1 imm12
              Fence -> error "fence not implemented"
              Csrrw -> error "csrrw not implemented"
              Csrrs -> error "csrrs not implemented"
              Csrrc -> error "csrrc not implemented"
              FenceI -> error "fencei not implemented"
              Csrrwi -> error "csrrwi not implemented"
              Csrrsi -> error "csrrsi not implemented"
              Csrrci -> error "csrrci not implemented"
              Lwu -> Instr64I $ MemInstr64 LWU rd imm12 rs1
              Ld -> Instr64I $ MemInstr64 LD rd imm12 rs1
              Addiw -> Instr64I $ ImmBinop64 ADDIW rd rs1 imm12
    SRepr | (rs1BV :< rs2BV :< imm12BV :< Nil) <- ops ->
            let rs1 = sizedBVToReg rs1BV
                rs2 = sizedBVToReg rs2BV
                imm12 = sizedBVToImm imm12BV
            in case oc of
              Sb -> undefined
              Sh -> undefined
              Sw -> undefined
              Sd -> undefined
    BRepr | (rs1BV :< rs2BV :< imm12BV :< Nil) <- ops ->
            let rs1 = sizedBVToReg rs1BV
                rs2 = sizedBVToReg rs2BV
                imm12 = sizedBVToImm imm12BV
            in case oc of
              Beq -> undefined
              Bne -> undefined
              Blt -> undefined
              Bltu -> undefined
              Bge -> undefined
              Bgeu -> undefined
    URepr | (rs1BV :< imm20BV :< Nil) <- ops ->
            let rs1 = sizedBVToReg rs1BV
                imm20 = sizedBVToImm imm20BV
            in case oc of
              Lui -> undefined
              Auipc -> undefined
    JRepr | (rdBV :< imm20BV :< Nil) <- ops ->
            let rd = sizedBVToReg rdBV
            in case oc of
              Jal -> undefined
    HRepr | (rdBV :< rs1BV :< shamt7BV :< Nil) <- ops ->
            let rd = sizedBVToReg rdBV
                rs1 = sizedBVToReg rs1BV
                shamt7 = sizedBVToImm shamt7BV
            in case oc of
              Slli -> undefined
              Srli -> undefined
              Srai -> undefined
    PRepr -> case oc of
      Ecall -> undefined
      Ebreak -> undefined
      Mret -> undefined
      Wfi -> undefined
    ARepr -> undefined
    R2Repr -> undefined
    R3Repr -> undefined
    R4Repr -> undefined
    RXRepr -> undefined
    XRepr -> case oc of
      Illegal -> undefined
