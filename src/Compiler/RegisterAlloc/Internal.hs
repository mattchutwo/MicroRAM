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

import           Compiler.IRs
import qualified MicroRAM.MicroRAM as MRAM

-- Retrieve the write registers of an instruction.
writeRegisters :: Ord reg => LTLInstr mdata reg wrdT -> Set reg
writeRegisters (MRI inst mdata) = writeRegistersMRIInstruction inst
writeRegisters (IRI inst mdata) = writeRegistersLTLInstruction inst

writeRegistersMRIInstruction :: Ord reg => MRAM.MAInstruction reg wrdT -> Set reg
writeRegistersMRIInstruction (MRAM.Iand r1 r2 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ior r1 r2 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ixor r1 r2 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Inot r1 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Iadd r1 r2 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Isub r1 r2 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Imull r1 r2 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Iumulh r1 r2 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ismulh r1 r2 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Iudiv r1 r2 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Iumod r1 r2 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ishl r1 r2 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ishr r1 r2 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Icmpe r1 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Icmpa r1 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Icmpae r1 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Icmpg r1 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Icmpge r1 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Imov r1 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Icmov r1 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ijmp op) = mempty
writeRegistersMRIInstruction (MRAM.Icjmp op) = mempty
writeRegistersMRIInstruction (MRAM.Icnjmp op) = mempty
writeRegistersMRIInstruction (MRAM.Istore op r1) = mempty -- writeRegistersOperand op
writeRegistersMRIInstruction (MRAM.Iload r1 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Iread r1 op) = Set.singleton r1
writeRegistersMRIInstruction (MRAM.Ianswer op) = mempty

writeRegistersLTLInstruction :: Ord reg => LTLInstr' reg mdata (MRAM.MAOperand reg wrdT) -> Set reg
writeRegistersLTLInstruction (Lgetstack s w t r1) = Set.singleton r1
writeRegistersLTLInstruction (Lsetstack r1 s w t) = mempty
writeRegistersLTLInstruction (LCall t mr op ts ops) = maybe mempty Set.singleton mr
writeRegistersLTLInstruction (LRet mo) = mempty
writeRegistersLTLInstruction (LAlloc mr t op) = maybe mempty Set.singleton mr

-- writeRegistersOperand :: Ord reg => MRAM.MAOperand reg wrdT -> Set reg
-- writeRegistersOperand (MRAM.Reg r) = Set.singleton r
-- writeRegistersOperand (MRAM.Const w) = mempty
-- writeRegistersOperand (MRAM.Label s) = mempty
-- writeRegistersOperand MRAM.HereLabel = mempty


-- Retrieve the read registers of an instruction.
readRegisters :: Ord reg => LTLInstr mdata reg wrdT -> Set reg
readRegisters (MRI inst mdata) = readRegistersMRIInstruction inst
readRegisters (IRI inst mdata) = readRegistersLTLInstruction inst

readRegistersMRIInstruction :: Ord reg => MRAM.MAInstruction reg wrdT -> Set reg
readRegistersMRIInstruction (MRAM.Iand r1 r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ior r1 r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ixor r1 r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Inot r1 op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Iadd r1 r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Isub r1 r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Imull r1 r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Iumulh r1 r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ismulh r1 r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Iudiv r1 r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Iumod r1 r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ishl r1 r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ishr r1 r2 op) = Set.singleton r2 <> readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icmpe r1 op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icmpa r1 op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icmpae r1 op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icmpg r1 op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icmpge r1 op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Imov r1 op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icmov r1 op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ijmp op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icjmp op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Icnjmp op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Istore op r1) = readRegistersOperand op <> Set.singleton r1
readRegistersMRIInstruction (MRAM.Iload r1 op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Iread r1 op) = readRegistersOperand op
readRegistersMRIInstruction (MRAM.Ianswer op) = readRegistersOperand op


readRegistersLTLInstruction :: Ord reg => LTLInstr' reg mdata (MRAM.MAOperand reg wrdT) -> Set reg
readRegistersLTLInstruction (Lgetstack s w t r1) = mempty
readRegistersLTLInstruction (Lsetstack r1 s w t) = Set.singleton r1
readRegistersLTLInstruction (LCall t mr op ts ops) = readRegistersOperand op <> mconcat (map readRegistersOperand ops)
readRegistersLTLInstruction (LRet mo) = maybe mempty readRegistersOperand mo
readRegistersLTLInstruction (LAlloc mr t op) = readRegistersOperand op

readRegistersOperand :: Ord reg => MRAM.MAOperand reg wrdT -> Set reg
readRegistersOperand (MRAM.Reg r) = Set.singleton r
readRegistersOperand (MRAM.Const w) = mempty
readRegistersOperand (MRAM.Label s) = mempty
readRegistersOperand (MRAM.Glob _ ) = mempty
readRegistersOperand MRAM.HereLabel = mempty


-- Substitute registers in an instruction.
substituteRegisters :: Ord reg => Map reg reg -> LTLInstr mdata reg wrdT -> LTLInstr mdata reg wrdT
substituteRegisters substs (MRI inst mdata) = MRI (substituteMRIInstruction substs inst) mdata
substituteRegisters substs (IRI inst mdata) = IRI (substituteLTLInstruction substs inst) mdata

substituteMRIInstruction :: Ord reg => Map reg reg -> MRAM.MAInstruction reg wrdT -> MRAM.MAInstruction reg wrdT
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

substituteLTLInstruction :: Ord reg => Map reg reg -> LTLInstr' reg mdata (MRAM.MAOperand reg wrdT) -> LTLInstr' reg mdata (MRAM.MAOperand reg wrdT)
substituteLTLInstruction substs (Lgetstack s w t r1) = Lgetstack s w t (substituteRegister substs r1)
substituteLTLInstruction substs (Lsetstack r1 s w t) = Lsetstack (substituteRegister substs r1) s w t
substituteLTLInstruction substs (LCall t mr op ts ops) = LCall t (substituteRegister substs <$> mr) (substituteOperand substs op) ts (map (substituteOperand substs) ops)
substituteLTLInstruction substs (LRet mo) = LRet (substituteOperand substs <$> mo)
substituteLTLInstruction substs (LAlloc mr t op) = LAlloc (substituteRegister substs <$> mr) t (substituteOperand substs op)

substituteRegister :: Ord reg => Map reg reg -> reg -> reg
substituteRegister substs r | Just r' <- Map.lookup r substs = r'
substituteRegister _      r                                  = r

substituteOperand :: Ord reg => Map reg reg -> MRAM.MAOperand reg wrdT -> MRAM.MAOperand reg wrdT
substituteOperand substs (MRAM.Reg r)   = MRAM.Reg (substituteRegister substs r)
substituteOperand _      (MRAM.Const w) = MRAM.Const w
substituteOperand _      (MRAM.Label s) = MRAM.Label s
substituteOperand _      MRAM.HereLabel = MRAM.HereLabel

