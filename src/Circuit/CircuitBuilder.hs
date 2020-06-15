{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Circuit.CircuitBuilder where

import Control.Monad.Identity
import Data.Coerce

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Circuit.CircuitIR

-- * Tags and tag generators

-- | State of used tags:
-- Every automatically generated is a string of an integer
-- and the state just carries the next integer.
-- Manual tags should not be numbers (not even start with numbers really)
type TState = State Int

initTState :: Int
initTState = 0

freshName :: State Int String
freshName = do
  n <- get
  put (n + 1)
  return $ show n

freshNameN :: Int -> State Int [String]
freshNameN 0 = do {return []}
freshNameN n = do
  name <- freshName
  names <- freshNameN (n-1)
  return $ name:names

evalTS :: TState t -> t
evalTS ts = evalState ts initTState

-- ** Building blocks of a processor

   

{- | Mux filter: simple filter that passes an input only
     if the signal matches a constant

   sig      X
    + +-+   +   +-+
    | |C|   |   |0|
    | +-+   |   +-+
    |  |cT  |    |
   +----+   |    |
   | EQ?|   |    | zeroT
   +----+   |    |
     |     +------+
     +-----+ mux  |
      eqT  +------+
              |
             out

out = if sig == C then X else 0

-}

muxFilter ::
  Int            -- ^ C
  -> String      -- ^ sig
  -> String      -- ^ X
  -> String      -- ^ out
  -> TState $ [TagGate Int String]
muxFilter c sig x out = do
  eqT <- freshName
  cT <- freshName
  zeroT <- freshName
  return $
    [gconst cT c              -- Constant C
    , gconst zeroT 0        -- Constant 0
    , geq eqT sig cT        -- sig == C
    , gmux out eqT x zeroT] -- if sig == C then x else 0


{- Not: not natural way for not:
   if x = 0 then 1
   else 0

   (It's really just a filter with one gate optimized away)  


      zeroT  +-+
sig +--------+0|
 +  |   +-+  +++
 |  |   |1|   |
 |  |   +++   |
 |  |    |    |
++--v+   |oneT|
| EQ?|   |    |zeroT
+-+--+   |    |
  |     +v----v+
  +-----+ mux  |
   eqT  +--+---+
           +
          out


  Notice that it doesn't enforce booleans, and interprets
  any number other than 0 as 1.


-}

notCircuit ::
  String        -- ^ inp
  -> String     -- ^ out
  -> TState $  [TagGate Int String]
{-
muxFilter ::
  Int            -- ^ C
  -> String      -- ^ sig
  -> String      -- ^ X
  -> String      -- ^ out
  -> TState $ [TagGate Int String] -}
  
notCircuit inp out = do
  eqT <- freshName
  oneT <- freshName
  zeroT <- freshName
  return $
    [ gconst zeroT 0        -- Constant 0
    , gconst oneT  1        -- Constant 1
    , geq eqT inp zeroT     -- sig == C
    , gmux out eqT oneT zeroT] -- if sig == 0 then 1 else 0
    
{- | Multiplexer:

          ins
       +  +  +  +
       |  |  |  |
       |  |  |  |
      +----------+
      |          |
 sig--+   mux    |
      |          |
      +----------+
           |
           |
          out

Picks the sig-th input from the list:
   out = Ins[sig]
If sig > len(Ins) : out = 0

(Don't confuse the gate multiplexer Gmux with the n input
multiplexer defined here)


-}

mux' ::
  Int           -- ^ initial value 
  -> [String]   -- ^ ins
  -> String     -- ^ sig
  -> String     -- ^ out
  -> TState $  [TagGate Int String]
mux' first ins sig out = do
  intermWires <- freshNameN (length ins) 
  tagNameIns <- return $ zip intermWires $ zip [first..] ins
  -- Create a muxFilter for each in
  filters <- mapM muxFilter' tagNameIns
  return $ gadd out intermWires :  -- sum the result of all wires
    (concat filters) 
  where muxFilter' (out, (n, inp)) = muxFilter n sig inp out

mux ::  
   [String]   -- ^ ins
  -> String     -- ^ sig
  -> String     -- ^ out
  -> TState $  [TagGate Int String]
mux  = mux' 0
  



{- | ALU:

         op1   op2
          +    +
          |    |
          |    |
       +----------+
       |          |
 inst--+   ALU    |
       |          |
       +----------+
            |  
            | 
           out

Computes the instruction indicated by inst over opt1 op2

out = Instructions[inst] op1 op2

Boolean instructions ashould be stored in the flag.
We will treat one of the registers as the flag. 

Instructions:
0 - Nothing (returns op2) for move instruction
1 - Addition
2 - substraction
3 - Multiplication
4 - Equality testing (flag)
5 - greater: op1 > op2 (flag)

(If you change the number of instructions, you have to change the jmp instructions
 defined in the processor.hs
TODO remove this dependency
)

-}

nopC,addC,mulC,eqC,gtC ::
  String    -- tag
  -> String -- op1 
  -> String -- op2
  -> [TagGate Int String] 

-- | 0. Nothing unit (return op2)
nopC tag op1 op2 = [grelay tag op2]
-- | 1. add unit (return op1+op2)
addC tag op1 op2 = [gadd tag [op1, op2]]
-- | 2. add unit (return op1+op2)
subC tag op1 op2 = [gsub tag [op1, op2]]
-- | 3. mull unit (return op1*op2)
mulC tag op1 op2 = [gmul tag [op1, op2]]
-- | 4. Equality testing unit (return op1 == op2)
eqC tag op1 op2 = [geq tag op1 op2]
-- | 5. Greater unit (return op1 > op2)
gtC tag op1 op2 = [ggt tag op1 op2]

-- | add unit
instructions = [nopC, addC, subC, mulC, eqC, gtC]
alu ::  
  String     -- ^ op1
  -> String     -- ^ op2
  -> String     -- ^ inst
  -> String     -- ^ out
  -> TState $  [TagGate Int String]
alu op1 op2 inst out = do
  unitOutputs <- freshNameN (length instructions) 
  allUnits <- return $ map (instantiateUnit op1 op2) $ zip instructions unitOutputs
  outputMux <- mux unitOutputs inst out
  return $ outputMux ++ (concat allUnits) 
  where instantiateUnit op1 op2 (inst, out) = inst out op1 op2




{- | OP1: just a mux

     +-------------+
     |  Registers  |
     +-----+-+-----+
        \  | |  /
         +-v-v-+
op1#---->+opmux|
         +--+--+
            |
            v
           Op1

Extracts the value in register # op1#

-}

op1Circuit ::  
   [String]   -- ^ registers
  -> String     -- ^ reg number
  -> String     -- ^ out
  -> TState $  [TagGate Int String]
op1Circuit registers opNumnber op1Tag =
  mux registers opNumnber op1Tag
  
{- | OP2: extracts op2 from registers
     but it could also be a Constant!

           +-------------+
           |  Registers  |
           +-----+-+-----+
              \  | |  /
               +-v-v-+
     op2# ---->+ mux |
               +--+--+
        C  --+    | 
             |    | 
          +--v----v--+
isConst -->   mux    |
          +----+-----+
               |
              Op2


-}

op2Circuit ::  
   [String]   -- ^ registers
  -> String     -- ^ reg2 number
  -> String     -- ^ Constant
  -> String     -- ^ is Constant?
  -> String     -- ^ out
  -> TState $  [TagGate Int String]
op2Circuit registers opNumber c isConst op2Tag = do
  firstPartOut <- freshName
  firstPart <- op1Circuit registers opNumber firstPartOut
  secondPart <- return $ gmux op2Tag isConst c firstPartOut
  return $ secondPart:firstPart
  
  
{- | New registers: sets the outReg to the value in the alu
     all other registers are set to their old value.
     If outReg > |regs| then new regs = old regs


          +-------------+
 aluOut   |  Registers  |
    |     +-------------+
    |        | | | | |
    +------+ | | | | |
           | | | | | |     
         +-v-v-v-v-v-v-+
         |             |
outReg--->             |
         +-------------+
             | | | | |
         +---v-v-v-v-v-+
         |New Registers|
         +-------------+

Spec: forall r, if r == ourReg
                then oldRegs[r] = aluOut
                else oldRegs[r] = newRegs 
-}

newRegsCircuit ::  
   [String]     -- ^ Old registers
  -> String     -- ^ aluOut
  -> String     -- ^ outReg
  -> [String]     -- ^ new registers (outputs)
  -> TState $  [TagGate Int String]
newRegsCircuit oldRegs aluOut outReg newRegs = do
  numberedPairedRegs <- return $ zip [0..] (zip oldRegs newRegs)
  allNewRegister <- mapM (newRegister' outReg aluOut) numberedPairedRegs 
  return $ concat allNewRegister
  where newRegister' outReg aluOut (rn, (oldR, newR)) =
          eqIf rn outReg aluOut oldR newR
  

{- | eqIf: choosing a wire, based on equality 
     this loos almost like the muxFilter, except instead of 0
     the defaul value is x.

   sign     x
    + +--+  +    y     
    | |rn|  |    +
    | +--+  |    |
    |  |rnT |    |
   +----+   |    |
   | EQ?|   |    | 
   +----+   |    |
     |     +------+
     +-----+ mux  |
      eqT  +------+
              |
             out

out = if sign == rn then x else y

-}
eqIf ::
  Int            -- ^ fixed number of this register
  -> String      -- ^ sign
  -> String      -- ^ x
  -> String      -- ^ y
  -> String      -- ^ out
  -> TState $ [TagGate Int String]
eqIf rn sign x y out = do
  eqT <- freshName
  rnT <- freshName
  return $
    [gconst rnT rn            -- Constant rn
    , geq eqT sign rnT        -- regN == rnT ?
    , gmux out eqT x y] -- if regN == rnT then aluOut else oldReg  


