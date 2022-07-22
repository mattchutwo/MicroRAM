-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeFamilyDependencies #-}

module Native where

-- import Compiler.Errors
-- import GHC.Generics
-- import MicroRAM
-- import MicroRAM.MRAMInterpreter.Generic

data NativeInstruction -- = NativeInstruction (forall i . Native i => Inst i)
--   deriving (Eq, Ord, Read, Show, Generic)
-- 
instance Eq NativeInstruction
instance Ord NativeInstruction
instance Read NativeInstruction
instance Show NativeInstruction
-- 
-- class (Eq arch, Ord arch, Read arch, Show arch, Generic arch) => Native arch where
--   type Inst arch = i | i -> arch
--   type State arch = s | s -> arch
-- 
--   -- | Convert a native instruction to a MRAM instruction.
--   toMRAMInsts :: Inst arch -> [Instruction r v]
-- 
--   stepArch :: State arch -> Inst arch -> Hopefully (State arch)
--   -- toArchState :: Regs r => MachineState' r v -> State arch
--   toArchState :: MachineState' r v -> State arch
-- 
--   -- | Whether two native states are equal (potentially ignore irrelevant fields).
--   archStateEq :: State arch -> State arch -> Bool

