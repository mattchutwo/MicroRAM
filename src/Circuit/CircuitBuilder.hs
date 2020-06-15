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
      egT  +------+
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

mux ::  
   [String]   -- ^ ins
  -> String     -- ^ sig
  -> String     -- ^ out
  -> TState $  [TagGate Int String]
mux ins sig out = do
  intermWires <- freshNameN (length ins) 
  tagNameIns <- return $ zip intermWires $ zip [0..] ins
  -- Create a muxFilter for each in
  filters <- mapM muxFilter' tagNameIns
  return $ gadd out intermWires :  -- sum the result of all wires
    (concat filters) 
  where muxFilter' (out, (n, inp)) = muxFilter n sig inp out




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
2 - Multiplication
3 - Equality testing (flag)
4 - greater: op1 > op2 (flag)

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
-- | 2. mull unit (return op1*op2)
mulC tag op1 op2 = [gmul tag [op1, op2]]
-- | 3. Equality testing unit (return op1 == op2)
eqC tag op1 op2 = [geq tag op1 op2]
-- | 4. Greater unit (return op1 > op2)
gtC tag op1 op2 = [ggt tag op1 op2]

-- | add unit
instructions = [nopC, addC, mulC, eqC, gtC]
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
          newReg rn outReg aluOut oldR newR
  

{- | new Register: for each new register 
     this loos almost like the muxFilter, except instead of 0
     the defaul value is oldReg

   regN    aluOut
    + +--+  +  oldReg    
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

out = if regN == rn then aluOut else oldReg

-}
newReg ::
  Int            -- ^ fixed number of this register
  -> String      -- ^ regN
  -> String      -- ^ aluOut
  -> String      -- ^ oldReg
  -> String      -- ^ out
  -> TState $ [TagGate Int String]
newReg rn regN aluOut oldReg out = do
  eqT <- freshName
  rnT <- freshName
  return $
    [gconst rnT rn            -- Constant rn
    , geq eqT regN rnT        -- regN == rnT ?
    , gmux out eqT aluOut oldReg] -- if regN == rnT then aluOut else oldReg  
  
{- Tests

lstNames :: String -> Int -> [String]
lstNames seed n = map (\n -> seed ++ show n) [0..(n-1)]



newNames = lstNames "newR"
oldNames = lstNames "oldR"

newRegsC :: Int -> TagCircuit Int String
newRegsC n = TC ("aluOut":"outReg":(oldNames n)) (evalTS $ newRegs (oldNames n) "aluOut" "outReg" (newNames n))

newRegsTestPure :: [Int] -> Int -> Int -> [Int]
newRegsTestPure [] _ _ = []
newRegsTestPure (_:ls) 0 a = (a:ls)
newRegsTestPure (hd:ls) n a = hd:(newRegsTestPure ls (n-1) a)
-}
