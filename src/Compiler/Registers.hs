{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Registers
Description : The class of registers we can use in MicroRAM
Maintainer  : santiago@galois.com
Stability   : alpha at best

The compiler backend is polymorphic on a register type. This allows
the compiler to choose the number of registers as well as the classes
(e.g. 32-bit, 64-bit, modulo P, etc...)

-}
module Compiler.Registers
    ( Regs(..),
      RegisterData(..)
    ) where


class Ord a => Regs a where
  -- Reserved registers
  sp :: a
  bp :: a
  -- caller-saved register
  ax :: a
  -- Register bank
  data RMap a :: * -> *
  initBank :: b -> RMap a b 
  lookupReg :: a -> RMap a b -> b
  updateBank :: a -> b -> RMap a b -> RMap a b


{- | RegisterData : carries info about the registers.
     Number of regs, classes, types.

     This is different from the Regs class. For example,
     we can implement registers indexed by `Int`s (instance of regs),
     but chose a different number of regs each time. That's what
     RegisterData is for.

     Needed once reg. alloc. is done.
-}

data RegisterData = RDempty
  deriving (Eq, Ord, Read, Show)
