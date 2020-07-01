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
      --RegBank(..)
    ) where


class Ord a => Regs a where
  -- Reserved registers
  sp :: a
  bp :: a
  -- caller-saved register
  ax :: a
  -- Arguments FIXME remove after reg alloc is fixed.
  argc :: a
  argv :: a
  -- Register bank
  data RMap a :: * -> *
  initBank :: b -> RMap a b 
  lookupReg :: a -> RMap a b -> b
  updateBank :: a -> b -> RMap a b -> RMap a b
  
-- | Register Banks
-- Are abstract bacuase in some cases we want them finite and fixed size
-- (generate a trace usefull by the circuit) and other cases we want it a variable
-- (e.g. to support compilation without register allocation)

{-
class (Show (b reg), Read (b reg)) => RegBank b reg | reg -> b where
  initBank :: b reg
  lookupReg :: reg -> b reg -> Word 
  updateBank :: reg -> Word -> b reg -> b reg
-}
  
