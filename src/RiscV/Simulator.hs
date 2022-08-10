{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Data.Parameterized.NatRepr
import Data.Parameterized.Some

import GRIFT.BitVector.BVApp (BVApp(..))
import GRIFT.BitVector.BVFloatApp (BVFloatApp(..))
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

instance RVStateM SimM RV64IM where
  getRV = return rv64IMRepr
  getPC = MS.gets mPC
  getGPR rid = (Vec.! asInt rid) <$> MS.gets mGPRs
  getFPR rid = undefined
  getMem bytes addr = do
    memory <- MS.gets mMemory
    let val = fmap (\a -> Map.findWithDefault 0 a memory)
              [addr..addr+(fromIntegral (natValue bytes-1))]
    return (bvConcatMany' ((knownNat @8) `natMultiply` bytes) val)

-- step :: Machine -> Instruction RV64G fmt -> Hopefully Machine
-- step m i = undefined
