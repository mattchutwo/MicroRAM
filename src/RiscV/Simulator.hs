{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RiscV.Simulator where

import qualified Control.Monad.State as MS
import           Data.Bits
import qualified Data.BitVector.Sized as BV
import           Data.BitVector.Sized (BV)
import qualified Data.BitVector.Sized.Unsigned as BVU
import           Data.BitVector.Sized.Unsigned (UnsignedBV(..))
import           Data.Foldable (foldl')
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Map.Merge.Strict as Map
import           Data.Type.Equality
import qualified Data.Vector as Vec
import           Data.Vector (Vector)
import           Numeric.Natural


import Control.Lens ((^.))
import Data.Parameterized.Classes
import Data.Parameterized.List
import Data.Parameterized.NatRepr
import Data.Parameterized.Some

import GHC.Word (Word64)

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
import Compiler.Registers
import MicroRAM (MWord)
import MicroRAM.MRAMInterpreter
import MicroRAM.MRAMInterpreter.Concrete (Concretizable(..))
import MicroRAM.MRAMInterpreter.Generic
import Native
import RiscV.RiscVAsm

import Debug.Trace

data RiscV

data Machine = Machine
  { mRV :: RVRepr RV64IM
  , mPC :: UnsignedBV (RVWidth RV64IM)
  , mMemory :: Map (UnsignedBV (RVWidth RV64IM)) (UnsignedBV 8)
  , mGPRs :: Vector (UnsignedBV (RVWidth RV64IM))
  }

prettyPrintMachine :: Machine -> String
prettyPrintMachine (Machine _rv pc mem regs) =
  "PC: " <> show pc <>
  "\nRegs: " <> show (openVB <$> regs) <>
  "\nMemory: " <> show (openVBMap mem)

openVB (UnsignedBV (BV n)) = n
openVBMap m = Map.mapKeys openVB $ Map.map openVB m

openGPRs :: Vector (UnsignedBV (RVWidth RV64IM)) -> Vector MWord
openGPRs v = fmap (fromIntegral . openVB) v

               
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
      iw = 1
  execSemantics (evalInstExpr knownISet i iw) semantics

stepInst :: Machine -> Some (Instruction RV64IM) -> Machine
stepInst m (Some i) =
  trace ("Taking step from:\n" <> prettyPrintMachine m <> "\n============") $
  MS.execState (unSimM (stepInstM i)) m

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

-- 
(=?=) :: (Show a, Eq a) => a -> a -> String -> String -> String -> Bool
(=?=) a1 a2 id1 id2 name =
  if a1 == a2 then True else
    trace ("The two " <> name <> "s don't match. \n\t" <> name <> "(" <> id1<> ") : " <> show a1 <> "\n\t" <> name <> "(" <> id2 <> ") : " <> show a2) False

instance Native RiscV where
  type Inst RiscV = Instr -- Some (Instruction RV64IM)
  type State RiscV = Machine

  stepArch m i = stepInst m <$> instrToGrift i

  toArchState :: forall v r.
                 (Concretizable v, Regs r)
              => MachineState' r v
              -> State RiscV
  toArchState ms' = Machine
    { mRV = knownRepr
    , mPC = UnsignedBV (BV.word64 (ms' ^. mPc))
    , mMemory = translateMem $ conMem @v (ms' ^. mMem)
    , mGPRs = Vec.fromList $ (UnsignedBV . BV.word64 . conGetValue) <$> regToList 32 (ms' ^. mRegs)
--  , mPC :: UnsignedBV (RVWidth RV64IM)
--  , mMemory :: Map (UnsignedBV (RVWidth RV64IM)) (UnsignedBV 8)
--  , mGPRs :: Vector (UnsignedBV (RVWidth RV64IM))
    }
    
  archStateEq s1 s2 =
    -- the PCs don't match: one RiscV instruction might be compiled to multiple MRAM ones
    -- so in the execution MRAM will increase the PC much more. 
    --((mPC s1 =?= mPC s2) "MRAM" "RiscV" "PC") &&
    (mMemory s1 `memoryEq` mMemory s2) && (mGPRs s1 `gprsEq` mGPRs s2)
    where
      memoryEq m1 m2 =
        let descriptiveDifference =
              Map.merge
              (Map.mapMaybeMissing $     \k x ->   if x==0 then Nothing else Just (x,0))
              (Map.mapMaybeMissing $     \k y ->   if 0==y then Nothing else Just (0,y))
              (Map.zipWithMaybeMatched $ \k x y -> if x==y then Nothing else Just (x,y))
              (openVBMap m1) (openVBMap m2)
        in if Map.null descriptiveDifference then True else
             trace ("Memories don't match. Here is a map with the differences where MRAM is shown first: \n " <> show descriptiveDifference)
             False

      gprsEq r1 r2 =
        if r1 == r2 then True else
          let r1' = openGPRs r1
              r2' = openGPRs r2
              descriptiveDifference =
                [(i, r1' Vec.! i, r2' Vec.! i) | i <- [0 .. 31], r1' Vec.! i /= r2' Vec.! i]
          in trace ("GPRs don't match. Here is a map with the differences where MRAM is shown first:\n" <> show descriptiveDifference)
             False
            


-- Mem translation is wrong. It should take into account the MRAM is word-aligned while GRIFT is byte-aligned.
translateMem :: Mem
             -> Map (UnsignedBV (RVWidth RV64IM)) (UnsignedBV 8)
translateMem (mDefault, mMap, _) = Map.fromList $ do
  (wordAddr, wordVal) <- Map.toList mMap
  offset <- [0..7]
  let byteAddr = wordAddr * 8 + fromIntegral offset
  let byteVal = (wordVal `shiftR` (8 * offset)) .&. 0xff
  return (UnsignedBV $ BV.word64 byteAddr, fromIntegral byteVal)

-- Not used right now TODO: Delete?
sizedBVToReg :: SizedBV 5 -> Reg
sizedBVToReg _ = undefined

-- Not used right now TODO: Delete?
sizedBVToImm :: SizedBV w -> Imm
sizedBVToImm _ = undefined

-- Not used right now TODO: Delete?
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
              Sb -> Instr32I $ MemInstr32 SB rs1 imm12 rs2
              Sh -> Instr32I $ MemInstr32 SH rs1 imm12 rs2
              Sw -> Instr32I $ MemInstr32 SW rs1 imm12 rs2
              Sd -> Instr64I $ MemInstr64 SD rs1 imm12 rs2
    BRepr | (rs1BV :< rs2BV :< imm12BV :< Nil) <- ops ->
            let rs1 = sizedBVToReg rs1BV
                rs2 = sizedBVToReg rs2BV
                imm12 = sizedBVToImm imm12BV
            in case oc of
              Beq -> Instr32I $ BranchInstr BEQ rs1 rs2 imm12
              Bne -> Instr32I $ BranchInstr BNE rs1 rs2 imm12
              Blt -> Instr32I $ BranchInstr BLT rs1 rs2 imm12
              Bltu -> Instr32I $ BranchInstr BLTU rs1 rs2 imm12
              Bge -> Instr32I $ BranchInstr BGE rs1 rs2 imm12
              Bgeu -> Instr32I $ BranchInstr BGEU rs1 rs2 imm12
    URepr | (rs1BV :< imm20BV :< Nil) <- ops ->
            let rs1 = sizedBVToReg rs1BV
                imm20 = sizedBVToImm imm20BV
            in case oc of
              Lui -> Instr32I $ LUI rs1 imm20
              Auipc -> Instr32I $ AUIPC rs1 imm20
    JRepr | (rdBV :< imm20BV :< Nil) <- ops ->
            let rd = sizedBVToReg rdBV
                imm20 = sizedBVToImm imm20BV
            in case oc of
              Jal -> Instr32I $ JAL rd imm20
    HRepr | (rdBV :< rs1BV :< shamt7BV :< Nil) <- ops ->
            let rd = sizedBVToReg rdBV
                rs1 = sizedBVToReg rs1BV
                shamt7 = sizedBVToImm shamt7BV
            in case oc of
              Slli -> Instr32I $ ImmBinop32 SLLI rd rs1 shamt7
              Srli -> Instr32I $ ImmBinop32 SRLI rd rs1 shamt7
              Srai -> Instr32I $ ImmBinop32 SRAI rd rs1 shamt7
    PRepr -> case oc of
      Ecall -> error "ecall not implemented"
      Ebreak -> error "ebreak not implemented"
      Mret -> error "mret not implemented"
      Wfi -> error "wfi not implemented"
    ARepr -> error "memory atomics not implemented"
    R2Repr -> error "r2-format instructions not implemented"
    R3Repr -> error "r3-format instructions not implemented"
    R4Repr -> error "r4-format instructions not implemented"
    RXRepr -> error "rx-format instructions not implemented"
    XRepr -> case oc of
      Illegal -> error "illegal instruction"

-- These functions to translate registers have a lot of duplicated code!
-- How can we factor them cleanly?
enumToSizedBV x =  sizedBVInteger $ toEnum $ fromEnum x
wordToSizedBV x =  sizedBVInteger $ toInteger x

regToSizedBV :: FormatRepr a -> Reg -> Reg -> Reg -> Operands a
regToSizedBV RRepr rd rs1 rs2  =
  let (rd', rs1', rs2') = (enumToSizedBV rd, enumToSizedBV rs1, enumToSizedBV rs2) in
    let ops = rd' :<  rs1' :< rs2' :< Nil in
      (Operands RRepr ops)
regToSizedBV IRepr rd rs1 rs2  =
  let (rd', rs1', rs2') = (enumToSizedBV rd, enumToSizedBV rs1, enumToSizedBV rs2) in
    let ops = rd' :<  rs1' :< rs2' :< Nil in
      (Operands IRepr ops)

-- catching errors if an immidiate is not a Number (i.e. a Word)
-- THIS IS NOT PURE!
withConcreteImm :: Imm -> (Word64 -> a) -> a
withConcreteImm imm f =
  case imm of
    ImmNumber w -> f w
    imm' -> error $ "RiscV emulator encountered an `Immediate` which is not concrete: " <> show imm' <> ".\n\tIf this is a `ImmLazy`, then it was probably missed by the `RemoveLabels` step. Otherwise it's a problem with the transpiler. "

regRegImmToSizedBV :: FormatRepr a -> Reg -> Reg -> Imm -> Operands a
regRegImmToSizedBV RRepr rd rs1 imm  =
  withConcreteImm imm $
  \w -> let (rd', rs1', imm') = (enumToSizedBV rd, enumToSizedBV rs1, wordToSizedBV w) in
          let ops = rd' :<  rs1' :< imm' :< Nil in
            (Operands RRepr ops)
regRegImmToSizedBV IRepr rd rs1 imm  =
  withConcreteImm imm $
  \w -> let (rd', rs1', imm') = (enumToSizedBV rd, enumToSizedBV rs1, wordToSizedBV w) in
          let ops = rd' :<  rs1' :< imm' :< Nil in
            (Operands IRepr ops)
regRegImmToSizedBV SRepr rd rs1 imm  =
  withConcreteImm imm $
  \w -> let (rd', rs1', imm') = (enumToSizedBV rd, enumToSizedBV rs1, wordToSizedBV w) in
          let ops = rd' :<  rs1' :< imm' :< Nil in
            (Operands SRepr ops)
regRegImmToSizedBV BRepr rd rs1 imm  =
  withConcreteImm imm $
  \w -> let (rd', rs1', imm') = (enumToSizedBV rd, enumToSizedBV rs1, wordToSizedBV w) in
          let ops = rd' :<  rs1' :< imm' :< Nil in
            (Operands BRepr ops)
              
regImmToSizedBV :: FormatRepr a -> Reg -> Imm -> Operands a
regImmToSizedBV URepr rs1 imm  =
  withConcreteImm imm $
  \w -> let (rs1', imm') = (enumToSizedBV rs1, wordToSizedBV w) in
          let ops = rs1' :< imm' :< Nil in
            (Operands URepr ops)
regImmToSizedBV JRepr rs1 imm  =
  withConcreteImm imm $
  \w -> let (rs1', imm') = (enumToSizedBV rs1, wordToSizedBV w) in
          let ops = rs1' :< imm' :< Nil in
            (Operands JRepr ops)



-- HERE
-- Not used right now TODO: Delete?
instrToGrift :: Instr -> Hopefully (Some (Instruction RV64IM))
instrToGrift instr =
  case instr of
    Instr32I (RegBinop32 ADD rd rs1 rs2) -> return $ Some $ Inst Add (regToSizedBV RRepr rd rs1 rs2)
    -- RRepr
    Instr32I (RegBinop32 ADD rd rs1 rs2  ) -> return $ Some $ Inst Add    (regToSizedBV RRepr rd rs1 rs2)    
    Instr32I (RegBinop32 SUB rd rs1 rs2  ) -> return $ Some $ Inst Sub    (regToSizedBV RRepr rd rs1 rs2)
    Instr32I (RegBinop32 SLL rd rs1 rs2  ) -> return $ Some $ Inst Sll    (regToSizedBV RRepr rd rs1 rs2)
    Instr32I (RegBinop32 SLT rd rs1 rs2  ) -> return $ Some $ Inst Slt    (regToSizedBV RRepr rd rs1 rs2)
    Instr32I (RegBinop32 SLTU rd rs1 rs2 ) -> return $ Some $ Inst Sltu   (regToSizedBV RRepr rd rs1 rs2)
    Instr32I (RegBinop32 XOR rd rs1 rs2  ) -> return $ Some $ Inst Xor    (regToSizedBV RRepr rd rs1 rs2)
    Instr32I (RegBinop32 SRL rd rs1 rs2  ) -> return $ Some $ Inst Srl    (regToSizedBV RRepr rd rs1 rs2)
    Instr32I (RegBinop32 SRA rd rs1 rs2  ) -> return $ Some $ Inst Sra    (regToSizedBV RRepr rd rs1 rs2)
    Instr32I (RegBinop32 OR rd rs1 rs2   ) -> return $ Some $ Inst Or     (regToSizedBV RRepr rd rs1 rs2)
    Instr32I (RegBinop32 AND rd rs1 rs2  ) -> return $ Some $ Inst And    (regToSizedBV RRepr rd rs1 rs2)
    Instr64I (RegBinop64 ADDW rd rs1 rs2 ) -> return $ Some $ Inst Addw   (regToSizedBV RRepr rd rs1 rs2)
    Instr64I (RegBinop64 SUBW rd rs1 rs2 ) -> return $ Some $ Inst Subw   (regToSizedBV RRepr rd rs1 rs2)
    Instr64I (RegBinop64 SLLW rd rs1 rs2 ) -> return $ Some $ Inst Sllw   (regToSizedBV RRepr rd rs1 rs2)
    Instr64I (RegBinop64 SRLW rd rs1 rs2 ) -> return $ Some $ Inst Srlw   (regToSizedBV RRepr rd rs1 rs2)
    Instr64I (RegBinop64 SRAW rd rs1 rs2 ) -> return $ Some $ Inst Sraw   (regToSizedBV RRepr rd rs1 rs2)
    Instr64I (ImmBinop64 SLLIW rd rs1 imm) -> return $ Some $ Inst Slliw  (regRegImmToSizedBV RRepr rd rs1 imm)
    Instr64I (ImmBinop64 SRLIW rd rs1 imm) -> return $ Some $ Inst Srliw  (regRegImmToSizedBV RRepr rd rs1 imm)
    Instr64I (ImmBinop64 SRAIW rd rs1 imm) -> return $ Some $ Inst Sraiw  (regRegImmToSizedBV RRepr rd rs1 imm)
    Instr32M (MUL rd rs1 rs2             ) -> return $ Some $ Inst Mul    (regToSizedBV RRepr rd rs1 rs2)
    Instr32M (MULH rd rs1 rs2            ) -> return $ Some $ Inst Mulh   (regToSizedBV RRepr rd rs1 rs2)
    Instr32M (MULHSU rd rs1 rs2          ) -> return $ Some $ Inst Mulhsu (regToSizedBV RRepr rd rs1 rs2)
    Instr32M (MULHU rd rs1 rs2           ) -> return $ Some $ Inst Mulhu  (regToSizedBV RRepr rd rs1 rs2)
    Instr32M (DIV rd rs1 rs2             ) -> return $ Some $ Inst Div    (regToSizedBV RRepr rd rs1 rs2)
    Instr32M (DIVU rd rs1 rs2            ) -> return $ Some $ Inst Divu   (regToSizedBV RRepr rd rs1 rs2)
    Instr32M (REM rd rs1 rs2             ) -> return $ Some $ Inst Rem    (regToSizedBV RRepr rd rs1 rs2)
    Instr32M (REMU rd rs1 rs2            ) -> return $ Some $ Inst Remu   (regToSizedBV RRepr rd rs1 rs2)
    Instr64M (MULW rd rs1 rs2            ) -> return $ Some $ Inst Mulw   (regToSizedBV RRepr rd rs1 rs2)
    Instr64M (DIVW rd rs1 rs2            ) -> return $ Some $ Inst Divw   (regToSizedBV RRepr rd rs1 rs2)
    Instr64M (DIVUW rd rs1 rs2           ) -> return $ Some $ Inst Divuw  (regToSizedBV RRepr rd rs1 rs2)
    Instr64M (REMW rd rs1 rs2            ) -> return $ Some $ Inst Remw   (regToSizedBV RRepr rd rs1 rs2)
    Instr64M (REMUW rd rs1 rs2           ) -> return $ Some $ Inst Remuw  (regToSizedBV RRepr rd rs1 rs2)
    -- IRepr 
    Instr32I (JALR rd rs1 imm12            ) -> return $ Some $ Inst Jalr  (regRegImmToSizedBV IRepr rd rs1 imm12)   
    Instr32I (MemInstr32 LB rd imm12 rs1   ) -> return $ Some $ Inst Lb    (regRegImmToSizedBV IRepr rd rs1 imm12)  
    Instr32I (MemInstr32 LH rd imm12 rs1   ) -> return $ Some $ Inst Lh    (regRegImmToSizedBV IRepr rd rs1 imm12)  
    Instr32I (MemInstr32 LW rd imm12 rs1   ) -> return $ Some $ Inst Lw    (regRegImmToSizedBV IRepr rd rs1 imm12)  
    Instr32I (MemInstr32 LBU rd imm12 rs1  ) -> return $ Some $ Inst Lbu   (regRegImmToSizedBV IRepr rd rs1 imm12)  
    Instr32I (MemInstr32 LHU rd imm12 rs1  ) -> return $ Some $ Inst Lhu   (regRegImmToSizedBV IRepr rd rs1 imm12)  
    Instr32I (ImmBinop32 ADDI rd rs1 imm12 ) -> return $ Some $ Inst Addi  (regRegImmToSizedBV IRepr rd rs1 imm12)  
    Instr32I (ImmBinop32 SLTI rd rs1 imm12 ) -> return $ Some $ Inst Slti  (regRegImmToSizedBV IRepr rd rs1 imm12)  
    Instr32I (ImmBinop32 SLTIU rd rs1 imm12) -> return $ Some $ Inst Sltiu (regRegImmToSizedBV IRepr rd rs1 imm12)  
    Instr32I (ImmBinop32 XORI rd rs1 imm12 ) -> return $ Some $ Inst Xori  (regRegImmToSizedBV IRepr rd rs1 imm12)  
    Instr32I (ImmBinop32 ORI rd rs1 imm12  ) -> return $ Some $ Inst Ori   (regRegImmToSizedBV IRepr rd rs1 imm12)  
    Instr32I (ImmBinop32 ANDI rd rs1 imm12 ) -> return $ Some $ Inst Andi  (regRegImmToSizedBV IRepr rd rs1 imm12)  
    Instr64I (MemInstr64 LWU rd imm12 rs1  ) -> return $ Some $ Inst Lwu   (regRegImmToSizedBV IRepr rd rs1 imm12)  
    Instr64I (MemInstr64 LD rd imm12 rs1   ) -> return $ Some $ Inst Ld    (regRegImmToSizedBV IRepr rd rs1 imm12)  
    Instr64I (ImmBinop64 ADDIW rd rs1 imm12) -> return $ Some $ Inst Addiw (regRegImmToSizedBV IRepr rd rs1 imm12)  
    -- SRepr | (rs1BV :< rs2BV :< imm12BV :< Nil) <- ops ->
    Instr32I (MemInstr32 SB rs1 imm12 rs2) -> return $ Some $ Inst Sb (regRegImmToSizedBV SRepr rs2 rs1 imm12)
    Instr32I (MemInstr32 SH rs1 imm12 rs2) -> return $ Some $ Inst Sh (regRegImmToSizedBV SRepr rs2 rs1 imm12)
    Instr32I (MemInstr32 SW rs1 imm12 rs2) -> return $ Some $ Inst Sw (regRegImmToSizedBV SRepr rs2 rs1 imm12)
    Instr64I (MemInstr64 SD rs1 imm12 rs2) -> return $ Some $ Inst Sd (regRegImmToSizedBV SRepr rs2 rs1 imm12)
    -- BRepr
    Instr32I (BranchInstr BEQ rs1 rs2 imm12 ) -> return $ Some $ Inst Beq  (regRegImmToSizedBV BRepr rs1 rs2 imm12)
    Instr32I (BranchInstr BNE rs1 rs2 imm12 ) -> return $ Some $ Inst Bne  (regRegImmToSizedBV BRepr rs1 rs2 imm12)
    Instr32I (BranchInstr BLT rs1 rs2 imm12 ) -> return $ Some $ Inst Blt  (regRegImmToSizedBV BRepr rs1 rs2 imm12)
    Instr32I (BranchInstr BLTU rs1 rs2 imm12) -> return $ Some $ Inst Bltu (regRegImmToSizedBV BRepr rs1 rs2 imm12)
    Instr32I (BranchInstr BGE rs1 rs2 imm12 ) -> return $ Some $ Inst Bge  (regRegImmToSizedBV BRepr rs1 rs2 imm12)
    Instr32I (BranchInstr BGEU rs1 rs2 imm12) -> return $ Some $ Inst Bgeu (regRegImmToSizedBV BRepr rs1 rs2 imm12)
    -- URepr
    Instr32I (LUI rs1 imm20  ) -> return $ Some $ Inst Lui   (regImmToSizedBV URepr rs1 imm20)
    Instr32I (AUIPC rs1 imm20) -> return $ Some $ Inst Auipc (regImmToSizedBV URepr rs1 imm20)
    -- JRepr 
    Instr32I (JAL rd imm20)    -> return $ Some $ Inst Jal   (regImmToSizedBV JRepr rd imm20)
    -- HRepr
    Instr32I (ImmBinop32 SLLI rd rs1 shamt7) -> return $ Some $ Inst Slli (regRegImmToSizedBV HRepr rd rs1 shamt7)
    Instr32I (ImmBinop32 SRLI rd rs1 shamt7) -> return $ Some $ Inst Srli (regRegImmToSizedBV HRepr rd rs1 shamt7)
    Instr32I (ImmBinop32 SRAI rd rs1 shamt7) -> return $ Some $ Inst Srai (regRegImmToSizedBV HRepr rd rs1 shamt7)
    -- Other
    i -> otherError $ "Instruction not supported: " <> show i
