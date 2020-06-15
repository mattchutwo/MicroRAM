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
import Circuit.CircuitBuilder

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

Functional Unit of the processpor:
    +--------------------+
    |   Old registers    +--+
    +----+-+-----+-+-+---+  |
         | |     | | |      |
reg1    +v-v+    | | |      |
+-------+OP1|    | | |      |
        +-+-+    | | |      |
reg2      |     +v-v-v+     |
+--------------->     |     |
constant  |     | OP2 |     |
+--------------->     |     |
isConst   |     |     |     |
+--------------->     |     |
          |     +-----+     |
      op1 |        |op2     |
        +-v--------v--+     |
inst    |             |     |
+------->    ALU      |     |
        +-------------+     |
               |aluOut      |
outReg     +---v--+         |
+----------> ret  <---------+
           ++-+-+-+
            | | | |
     +------v-v-v-v------+
     |  New registers    |
     +-------------------+


All the pieces are defined in CircuitBuilder.hs, just need to plug them in.
TODO

-}

functionalUnit ::
  [String]            -- ^ Old registers
  -> String      -- ^ reg1
  -> String      -- ^ reg2
  -> String      -- ^ constant
  -> String      -- ^ isConst
  -> String      -- ^ inst
  -> String      -- ^ outReg
  -> [String]      -- ^ New registers (output)
  -> TState $ [TagGate Int String]
functionalUnit oldRegs reg1 reg2 const isConst inst outReg newRegs = do
  op1 <- freshName
  op2 <- freshName
  aluOut <- freshName
  op1C <- op1Circuit oldRegs reg1 op1
  op2C <- op2Circuit oldRegs reg2 const isConst op2
  aluC <- alu op1 op2 inst aluOut
  retC <- newRegsCircuit oldRegs aluOut outReg newRegs
  return $ concat [op1C,op2C,aluC,retC]

  
