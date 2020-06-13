{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Circuit.Processor where

import Control.Monad.Identity
import Data.Coerce

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Circuit.CircuitIR

-- * Tags and tag generators

-- | Unit: describes some units of the processor to tag them
data Unit =
  ALU
  | Operands
  | Decoder
  | RegStore
  | Other String
  deriving (Eq, Ord, Show, Read)

-- | In a processor every tag indicates what unit it belongs to
data Tag =
  Tag Unit String
  deriving (Eq, Ord, Show, Read)


{-
    +--------------------+
    |   Old registers    |
    +------------+-+-+---+
         | |     | | |
op1#    +v-v+    | | |
+-------+OP1|    | | |
        +-+-+    | | |
op2#      |     +v-v-v+
+--------------->     |
constant  |     | OP2 |
+--------------->     |
isConst   |     |     |
+--------------->     |
          |     +--+--+
          |        |
        +-v--------v--+
        |             |
        |    ALU      |
        +------+------+
               |
outReg     +---v--+
+----------> ret  |
           ++-+-+-+
            | | | |
     +------v-v-v-v------+
     |  New registers    |
     +-------------------+

All the pieces are defined in CircuitBuilder.hs, just need to plug them in.
TODO

-}

