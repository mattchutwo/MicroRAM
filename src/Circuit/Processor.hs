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
  -> String     -- ^ n
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
