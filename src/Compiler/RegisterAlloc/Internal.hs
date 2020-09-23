{-# LANGUAGE GADTs #-}

module Compiler.RegisterAlloc.Internal (
    writeRegisters
  , readRegisters
  , substituteRegisters
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

-- import           Compiler.Registers
import           Compiler.IRs
import qualified MicroRAM as MRAM

-- Retrieve the write registers of an instruction.

writeRegisters :: Ord reg => LTLInstr mdata reg wrdT -> Set reg
writeRegisters (MRI inst _mdata) = writeRegistersMRIInstruction inst
writeRegisters (IRI inst _mdata) = writeRegistersLTLInstruction inst

writeRegistersMRIInstruction :: Ord reg => MAInstruction reg wrdT -> Set reg
writeRegistersMRIInstruction (MRAM.Iand r1 _r2 _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ior r1 _r2 _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ixor r1 _ _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Inot r1 _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Iadd r1 _ _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Isub r1 _ _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Imull r1 _ _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Iumulh r1 _ _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ismulh r1 _ _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Iudiv r1 _ _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Iumod r1 _ _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ishl r1 _ _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ishr r1 _ _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Icmpe _ _) = mempty
writeRegistersMRIInstruction (MRAM.Icmpa _ _) = mempty
writeRegistersMRIInstruction (MRAM.Icmpae _ _) = mempty
writeRegistersMRIInstruction (MRAM.Icmpg _ _) = mempty
writeRegistersMRIInstruction (MRAM.Icmpge _ _) = mempty
writeRegistersMRIInstruction (MRAM.Imov r1 _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Icmov r1 _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ijmp _) = mempty
writeRegistersMRIInstruction (MRAM.Icjmp _) = mempty
writeRegistersMRIInstruction (MRAM.Icnjmp _) = mempty
writeRegistersMRIInstruction (MRAM.Istore _ _) = mempty -- writeRegistersOperand op
writeRegistersMRIInstruction (MRAM.Iload r1 _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Iread r1 _) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ianswer _) = mempty
writeRegistersMRIInstruction (MRAM.Ipoison _ _) = mempty
writeRegistersMRIInstruction (MRAM.Iadvise rd) = Set.singleton rd
writeRegistersMRIInstruction (MRAM.Iext _ _) = mempty
writeRegistersMRIInstruction (MRAM.Iextval _ rd _) = Set.singleton rd
writeRegistersMRIInstruction (MRAM.Iextadvise _ rd _) = Set.singleton rd

writeRegistersLTLInstruction :: Ord reg => LTLInstr' reg mdata (MAOperand reg wrdT) -> Set reg
writeRegistersLTLInstruction (Lgetstack _ _ _ r1) = Set.singleton r1
writeRegistersLTLInstruction (Lsetstack _ _ _ _) = mempty
writeRegistersLTLInstruction (LCall _t mr _ _ _) = maybe mempty Set.singleton mr
writeRegistersLTLInstruction (LRet _mo) = mempty
writeRegistersLTLInstruction (LAlloc mr _t _op) = maybe mempty Set.singleton mr

-- writeRegistersOperand :: Ord reg => MRAM.MAOperand reg wrdT -> Set reg
-- writeRegistersOperand (MRAM.Reg r) = Set.singleton r
-- writeRegistersOperand (MRAM.Const w) = mempty
-- writeRegistersOperand (MRAM.Label s) = mempty
-- writeRegistersOperand MRAM.HereLabel = mempty


-- Retrieve the read registers of an instruction.
readRegisters :: Ord reg => LTLInstr mdata reg wrdT -> Set reg
readRegisters (MRI inst _mdata) = readRegistersMRIInstruction inst
readRegisters (IRI inst _mdata) = readRegistersLTLInstruction inst

readRegistersMRIInstruction :: Ord reg => MAInstruction reg wrdT -> Set reg
readRegistersMRIInstruction (MRAM.Iand _ r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ior _ r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ixor _ r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Inot _ op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Iadd _ r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Isub _ r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Imull _ r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Iumulh _ r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ismulh _ r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Iudiv _ r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Iumod _ r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ishl _ r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ishr _ r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icmpe r1 op) = Set.singleton r1 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icmpa r1 op) = Set.singleton r1 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icmpae r1 op) = Set.singleton r1 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icmpg r1 op) = Set.singleton r1 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icmpge r1 op) = Set.singleton r1 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Imov _ op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icmov _ op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ijmp op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icjmp op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icnjmp op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Istore op r1) = readRegistersOperand op <> Set.singleton r1
readRegistersMRIInstruction (MRAM.Iload _ op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Iread _ op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ianswer op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ipoison op r1) = readRegistersOperand op <> Set.singleton r1
readRegistersMRIInstruction (MRAM.Iadvise _) = Set.empty
readRegistersMRIInstruction (MRAM.Iext _ ops) = Set.unions $ map readRegistersOperand ops
readRegistersMRIInstruction (MRAM.Iextval _ _ ops) = Set.unions $ map readRegistersOperand ops
readRegistersMRIInstruction (MRAM.Iextadvise _ _ ops) = Set.unions $ map readRegistersOperand ops


readRegistersLTLInstruction :: Ord reg => LTLInstr' reg mdata (MAOperand reg wrdT) -> Set reg
readRegistersLTLInstruction (Lgetstack _ _ _ _) = mempty
readRegistersLTLInstruction (Lsetstack r1 _ _ _) = Set.singleton r1
readRegistersLTLInstruction (LCall _t _mr op _ts ops) = readRegistersOperand op <> mconcat (map readRegistersOperand ops)
readRegistersLTLInstruction (LRet mo) = maybe mempty readRegistersOperand mo
readRegistersLTLInstruction (LAlloc _mr _t op) = readRegistersOperand op

readRegistersOperand :: Ord reg => MAOperand reg wrdT -> Set reg
readRegistersOperand (AReg r) = Set.singleton r
readRegistersOperand (LImm _w) = mempty
readRegistersOperand (Label _s) = mempty
readRegistersOperand (Glob _ ) = mempty
readRegistersOperand HereLabel = mempty


-- Substitute registers in an instruction.
substituteRegisters :: Ord reg => Map reg reg -> LTLInstr mdata reg wrdT -> LTLInstr mdata reg wrdT
substituteRegisters substs (MRI inst mdata) = MRI (substituteMRIInstruction substs inst) mdata
substituteRegisters substs (IRI inst mdata) = IRI (substituteLTLInstruction substs inst) mdata

substituteMRIInstruction :: Ord reg => Map reg reg -> MAInstruction reg wrdT -> MAInstruction reg wrdT
substituteMRIInstruction substs (MRAM.Iand r1 r2 op) = MRAM.Iand (substituteRegister substs r1) (substituteRegister substs r2) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Ior r1 r2 op) = MRAM.Ior (substituteRegister substs r1) (substituteRegister substs r2) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Ixor r1 r2 op) = MRAM.Ixor (substituteRegister substs r1) (substituteRegister substs r2) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Inot r1 op) = MRAM.Inot (substituteRegister substs r1) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Iadd r1 r2 op) = MRAM.Iadd (substituteRegister substs r1) (substituteRegister substs r2) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Isub r1 r2 op) = MRAM.Isub (substituteRegister substs r1) (substituteRegister substs r2) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Imull r1 r2 op) = MRAM.Imull (substituteRegister substs r1) (substituteRegister substs r2) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Iumulh r1 r2 op) = MRAM.Iumulh (substituteRegister substs r1) (substituteRegister substs r2) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Ismulh r1 r2 op) = MRAM.Ismulh (substituteRegister substs r1) (substituteRegister substs r2) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Iudiv r1 r2 op) = MRAM.Iudiv (substituteRegister substs r1) (substituteRegister substs r2) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Iumod r1 r2 op) = MRAM.Iumod (substituteRegister substs r1) (substituteRegister substs r2) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Ishl r1 r2 op) = MRAM.Ishl (substituteRegister substs r1) (substituteRegister substs r2) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Ishr r1 r2 op) = MRAM.Ishr (substituteRegister substs r1) (substituteRegister substs r2) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Icmpe r1 op) = MRAM.Icmpe (substituteRegister substs r1) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Icmpa r1 op) = MRAM.Icmpa (substituteRegister substs r1) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Icmpae r1 op) = MRAM.Icmpae (substituteRegister substs r1) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Icmpg r1 op) = MRAM.Icmpg (substituteRegister substs r1) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Icmpge r1 op) = MRAM.Icmpge (substituteRegister substs r1) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Imov r1 op) = MRAM.Imov (substituteRegister substs r1) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Icmov r1 op) = MRAM.Icmov (substituteRegister substs r1) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Ijmp op) = MRAM.Ijmp (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Icjmp op) = MRAM.Icjmp (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Icnjmp op) = MRAM.Icnjmp (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Istore op r1) = MRAM.Istore (substituteOperand substs op) (substituteRegister substs r1)
substituteMRIInstruction substs (MRAM.Iload r1 op) = MRAM.Iload (substituteRegister substs r1) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Iread r1 op) = MRAM.Iread (substituteRegister substs r1) (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Ianswer op) = MRAM.Ianswer (substituteOperand substs op)
substituteMRIInstruction substs (MRAM.Ipoison op r1) = MRAM.Istore (substituteOperand substs op) (substituteRegister substs r1)
substituteMRIInstruction substs (MRAM.Iadvise r1) = MRAM.Iadvise (substituteRegister substs r1)
substituteMRIInstruction substs (MRAM.Iext name ops) = MRAM.Iext name (map (substituteOperand substs) ops)
substituteMRIInstruction substs (MRAM.Iextval name rd ops) = MRAM.Iextval name (substituteRegister substs rd) (map (substituteOperand substs) ops)
substituteMRIInstruction substs (MRAM.Iextadvise name rd ops) = MRAM.Iextadvise name (substituteRegister substs rd) (map (substituteOperand substs) ops)

substituteLTLInstruction :: Ord reg => Map reg reg -> LTLInstr' reg mdata (MAOperand reg wrdT) -> LTLInstr' reg mdata (MAOperand reg wrdT)
substituteLTLInstruction substs (Lgetstack s w t r1) = Lgetstack s w t (substituteRegister substs r1)
substituteLTLInstruction substs (Lsetstack r1 s w t) = Lsetstack (substituteRegister substs r1) s w t
substituteLTLInstruction substs (LCall t mr op ts ops) = LCall t (substituteRegister substs <$> mr) (substituteOperand substs op) ts (map (substituteOperand substs) ops)
substituteLTLInstruction substs (LRet mo) = LRet (substituteOperand substs <$> mo)
substituteLTLInstruction substs (LAlloc mr t op) = LAlloc (substituteRegister substs <$> mr) t (substituteOperand substs op)

substituteRegister :: Ord reg => Map reg reg -> reg -> reg
substituteRegister substs r | Just r' <- Map.lookup r substs = r'
substituteRegister _      r                                  = r

substituteOperand :: Ord reg => Map reg reg -> MAOperand reg wrdT -> MAOperand reg wrdT
substituteOperand substs (AReg r)   = AReg (substituteRegister substs r)
substituteOperand _      (LImm w) = LImm w
substituteOperand _      (Label s) = Label s
substituteOperand _      (Glob g) = Glob g
substituteOperand _      HereLabel = HereLabel

