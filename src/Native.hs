{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Native where

import Compiler.Errors
import Data.Typeable
import GHC.Generics
import MicroRAM
import MicroRAM.MRAMInterpreter.Generic

data NativeInstruction = forall i . Native i => NativeInstruction (Inst i)

instance Eq NativeInstruction where
  (NativeInstruction (i1 :: a)) == (NativeInstruction (i2 :: b)) = case eqT :: Maybe (a :~: b) of
    Nothing -> False
    Just Refl -> i1 == i2

instance Ord NativeInstruction where
  compare (NativeInstruction (i1 :: a)) (NativeInstruction (i2 :: b)) = case eqT :: Maybe (a :~: b) of
    Nothing -> error "Cannot compare different native instructions" -- JP: Could define an ordering over known architectures..
    Just Refl -> compare i1 i2

instance Read NativeInstruction where
  readsPrec _ = error "Cannot read a native instruction" -- JP: We could for known architectures...

instance Show NativeInstruction where
  show (NativeInstruction i) = show i


class (Typeable (Inst arch), Eq (Inst arch), Ord (Inst arch), Show (Inst arch)) => Native arch where
  type Inst arch = i | i -> arch
  type State arch = s | s -> arch

  -- | Convert a native instruction to a MRAM instruction.
  toMRAMInsts :: Inst arch -> [Instruction r v]

  stepArch :: State arch -> Inst arch -> Hopefully (State arch)
  -- toArchState :: Regs r => MachineState' r v -> State arch
  toArchState :: MachineState' r v -> State arch

  -- | Whether two native states are equal (potentially ignore irrelevant fields).
  archStateEq :: State arch -> State arch -> Bool

