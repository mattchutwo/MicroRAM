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
+--------------->     +-------> op2out
isConst   |     |     |     |
+--------------->     |     |
          |     +-----+     |
      op1 |        |op2out  |
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

Op2 is outputed because it is used by the pc calculator and the memory unit.

All the pieces are defined in CircuitBuilder.hs, just need to plug them in.
TODO

-}

functionalUnit ::
  [String]       -- ^ Old registers
  -> String      -- ^ reg1
  -> String      -- ^ reg2
  -> String      -- ^ constant
  -> String      -- ^ isConst
  -> String      -- ^ inst
  -> String      -- ^ outReg
  -> String      -- ^ op2out
  -> [String]      -- ^ New registers (output)
  -> TState $ [TagGate Int String]
functionalUnit oldRegs reg1 reg2 const isConst inst outReg op2out newRegs = do
  op1 <- freshName
  aluOut <- freshName
  op1C <- op1Circuit oldRegs reg1 op1
  op2C <- op2Circuit oldRegs reg2 const isConst op2out
  aluC <- alu op1 op2out inst aluOut
  retC <- newRegsCircuit oldRegs aluOut outReg newRegs
  return $ concat [op1C,op2C,aluC,retC]

  
{- | pcCircuit: computes the new pc. As follows
   if instr = jump then op2
   elif instr == cjump && flag then op2
   elif instr == cnjump && ~ flag then op 
   else pc+1
               oldPc
                 + 
            op2  | 1
             +   | |
    flag     | +-v-v-+
instr +      | | add |
+---+ |      | +--+--+
    | |      |    |
    | |      +--+ | pc1
  +-v-v-+ jmp   | |
  | jmp | Dec +-v-v-+
  | Dec +-----> mux |
  +-----+     +--+--+
                |
                v
               newPc



-}

pcCircuit ::  
    Int         -- ^ jump intruction number
  -> Int         -- ^ cjump intruction number
  -> Int         -- ^ cnjump intruction number
  -> String      -- ^ instr
  -> String     -- ^ flag
  -> String     -- ^ op2
  -> String     -- ^ oldPc
  -> String     -- ^ newPc
  -> TState $  [TagGate Int String]
pcCircuit jmp cjmp cnjmp instr flag op2 oldPc newPc = do
  one <- freshName
  pc1 <- freshName
  pc1C <- return $
    [ TG one (Gconst 1),
      TG pc1 (Gadd [oldPc,one])] -- calculates pc + 1
  jmpDec <- freshName  
  jmpDecC <- jmpDecCircuit jmp cjmp cnjmp instr flag jmpDec
  jumpMuxC <- return [gmux newPc jmpDec op2 pc1]
  return $ concat [pc1C,jmpDecC, jumpMuxC]

    
{- Jump decider: calculates
   if instr = jump then 1
   elif instr == cjump then flag
   elif instr == cnjump then ~flag
   else 0
   

              flag
        +---+   |      jmp
        |not<---+       |
        +-+-+   |  +----v-+
instr     |     |  |EQ?   |
+-+---------------->Jump  |
  |       |     |  +------+
  |   not |     |   | dec1
  |   Flag|   +-v---v+
  |       |   |eqIF  |
  +----------->cjump |
  |       |   ++-----+
  |       |    | dec2
  |      +v----v+
  |      |eqIf  |
  +------>cnjump+----> out
         +------+
               

-}

jmpDecCircuit ::
    Int         -- ^ jump intruction number
  -> Int         -- ^ cjump intruction number
  -> Int         -- ^ cnjump intruction number
  -> String      -- ^ instr
  -> String     -- ^ flag
  -> String     -- ^ out
  -> TState $  [TagGate Int String]
jmpDecCircuit jmp cjmp cnjmp instr flag out = do
  dec1 <- freshName
  jmpT <- freshName
  jmpC <- return $ [gconst jmpT jmp]
  eq1C <- return $ [geq dec1 instr jmpT]
  dec2 <- freshName
  eqIfC1 <- eqIf cjmp instr flag dec1 dec2
  notFlag <- freshName
  notC <- notCircuit notFlag flag
  eqIfC2 <- eqIf cnjmp instr notFlag dec2 out
  return $ concat [jmpC, eq1C, eqIfC1, notC, eqIfC2]

{-
 Processor:

                      +------+
      +-------------+ |old pc|
      |old registers| +---+--+
      +-----+-------+     |
            |       |     |
reg1   +----v-----+ |     |
+------>          | |     |
reg2   |          | |     |
+------>Functional| |     |
const. |   Unit   | |     |
+------>          | |     |
isConst|          | |flag |
+------>          | +--+  |
outReg |          |   +v--v+
+------>          |op2|    |
inst   |          +--->    |
++----->          |   | pc |
 |     +----+-----+   |Circ|
 |          |         |    |
 +-------------------->    |
            |         +--+-+
            v            |
      +-----+-------+ +--v---+
      |old registers| |new pc|
      +-------------+ +------+


We hard-code the following
* There are 4 alu instrucitons
* jmpInstr = 5
* cjmpInstr = 6
* cnjmpInstr = 7
* flag is the last register
  flag = (length regs) - 1 
-}
jmpInstr = 5
cjmpInstr = jmpInstr + 1
cnjmpInstr = jmpInstr + 2

processor ::
  [String]       -- ^ Old registers
  -> String      -- ^ reg1
  -> String      -- ^ reg2
  -> String      -- ^ constant
  -> String      -- ^ isConst
  -> String      -- ^ inst
  -> String      -- ^ outReg
  -> String      -- ^ oldPc
  -> String      -- ^ newPc
  -> [String]      -- ^ New registers (output)
  -> TState $ [TagGate Int String]
processor oldRegs reg1 reg2 const isConst instr outReg oldPc newPc newRegs = do
  op2 <- freshName
  funcUnitC <- functionalUnit oldRegs reg1 reg2 const isConst instr outReg op2 newRegs
  pcC <- pcCircuit jmpInstr cjmpInstr cnjmpInstr instr flag op2 oldPc newPc
  return $ concat [funcUnitC, pcC]
  where flag = oldRegs !! ((length oldRegs) - 1)
