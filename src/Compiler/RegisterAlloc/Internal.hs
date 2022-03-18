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
writeRegistersMRIInstruction instr = MRAM.foldInstr Set.singleton (\_-> mempty) (\_-> mempty) instr

writeRegistersLTLInstruction :: Ord reg => LTLInstr' reg mdata (MAOperand reg wrdT) -> Set reg
writeRegistersLTLInstruction (Lgetstack _ _ _ r1) = Set.singleton r1
writeRegistersLTLInstruction (Lsetstack _ _ _ _) = mempty
writeRegistersLTLInstruction (LCall _t mr _ _ _) = maybe mempty Set.singleton mr
writeRegistersLTLInstruction (LRet _mo) = mempty
writeRegistersLTLInstruction (LAlloc mr _t _op) = maybe mempty Set.singleton mr
writeRegistersLTLInstruction (LGetBP r) = Set.singleton r

-- Retrieve the read registers of an instruction.
readRegisters :: Ord reg => LTLInstr mdata reg wrdT -> Set reg
readRegisters (MRI inst _mdata) = readRegistersMRIInstruction inst
readRegisters (IRI inst _mdata) = readRegistersLTLInstruction inst

readRegistersMRIInstruction :: Ord reg => MAInstruction reg wrdT -> Set reg
readRegistersMRIInstruction instr = MRAM.foldInstr returnReg Set.singleton readRegistersOperand  instr
  where  -- For Icmov, the return register is live (think of 'cmov a b c'  as `a <- b ? a : c`)
         -- For all other instructions the return register is not live (unless it is used as an operand too)
    returnReg = case instr of
      MRAM.Icmov {} -> Set.singleton
      _ -> const mempty

      
readRegistersLTLInstruction :: Ord reg => LTLInstr' reg mdata (MAOperand reg wrdT) -> Set reg
readRegistersLTLInstruction (Lgetstack _ _ _ _) = mempty
readRegistersLTLInstruction (Lsetstack r1 _ _ _) = Set.singleton r1
readRegistersLTLInstruction (LCall _t _mr op _ts ops) = readRegistersOperand op <> mconcat (map readRegistersOperand ops)
readRegistersLTLInstruction (LRet mo) = maybe mempty readRegistersOperand mo
readRegistersLTLInstruction (LAlloc _mr _t op) = readRegistersOperand op
readRegistersLTLInstruction (LGetBP _r) = mempty

readRegistersOperand :: Ord reg => MAOperand reg wrdT -> Set reg
readRegistersOperand (AReg r) = Set.singleton r
readRegistersOperand (LImm _w) = mempty
readRegistersOperand (Label _s) = mempty
readRegistersOperand HereLabel = mempty


-- Substitute registers in an instruction.
substituteRegisters :: Ord reg => Map reg reg -> LTLInstr mdata reg wrdT -> LTLInstr mdata reg wrdT
substituteRegisters substs (MRI inst mdata) = MRI (substituteMRIInstruction substs inst) mdata
substituteRegisters substs (IRI inst mdata) = IRI (substituteLTLInstruction substs inst) mdata

substituteMRIInstruction :: Ord reg => Map reg reg -> MAInstruction reg wrdT -> MAInstruction reg wrdT
substituteMRIInstruction substs instr = MRAM.mapInstr (substituteRegister substs) (substituteRegister substs) (substituteOperand substs) instr

substituteLTLInstruction :: Ord reg => Map reg reg -> LTLInstr' reg mdata (MAOperand reg wrdT) -> LTLInstr' reg mdata (MAOperand reg wrdT)
substituteLTLInstruction substs (Lgetstack s w t r1) = Lgetstack s w t (substituteRegister substs r1)
substituteLTLInstruction substs (Lsetstack r1 s w t) = Lsetstack (substituteRegister substs r1) s w t
substituteLTLInstruction substs (LCall t mr op ts ops) = LCall t (substituteRegister substs <$> mr) (substituteOperand substs op) ts (map (substituteOperand substs) ops)
substituteLTLInstruction substs (LRet mo) = LRet (substituteOperand substs <$> mo)
substituteLTLInstruction substs (LAlloc mr t op) = LAlloc (substituteRegister substs <$> mr) t (substituteOperand substs op)
substituteLTLInstruction substs (LGetBP r) = LGetBP (substituteRegister substs r)

substituteRegister :: Ord reg => Map reg reg -> reg -> reg
substituteRegister substs r | Just r' <- Map.lookup r substs = r'
substituteRegister _      r                                  = r

substituteOperand :: Ord reg => Map reg reg -> MAOperand reg wrdT -> MAOperand reg wrdT
substituteOperand substs (AReg r)   = AReg (substituteRegister substs r)
substituteOperand _      (LImm w) = LImm w
substituteOperand _      (Label s) = Label s
substituteOperand _      HereLabel = HereLabel

