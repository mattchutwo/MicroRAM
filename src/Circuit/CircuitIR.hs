{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Circuit.CircuitIR where

import Control.Monad.Identity
import Data.Coerce

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
type ($) a b = a b


data Gate val wire =
--  Gin Int               -- ^ input (indext to an input list)
   Gconst val          -- ^ constant
  | Gadd [wire]         -- ^ adds all wires in a list
  | Gmul [wire]         -- ^ multiplies all wires in a list
  | Gsub wire wire      -- ^ substracts two values
  | Gmux wire wire wire -- ^ if a then b else c
  | Geq  wire wire      -- ^ a == b
  | Ggt  wire wire      -- ^ a == b
  | Gge  wire wire      -- ^ a == b 
  | Grelay wire         -- ^ useful, but optimize away!
  | Ggadget String [wire]  -- ^ Eventually we might want gadgets with multiple outs?  
  deriving (Eq, Show, Read, Functor, Traversable, Foldable)


-- usefull constructors
gconst tag val = TG tag (Gconst val)
gadd tag tags = TG tag (Gadd tags)
gsub tag x y = TG tag (Gsub x y)
gmul tag tags = TG tag (Gmul tags)
gmux tag w1 w2 w3 = TG tag (Gmux w1 w2 w3)
geq tag w1 w2 = TG tag (Geq w1 w2)
ggt tag w1 w2 = TG tag (Ggt w1 w2)
gge tag w1 w2 = TG tag (Gge w1 w2)
grelay tag w1 = TG tag (Grelay w1)
ggadget tag name tags = TG tag (Ggadget name tags)

-- There are two kinds of circuits:
-- tagCircuits : where gates are tagged and wires are tags
-- Circuits : which are (assumed) topologically sorter, so wires are inices

-- | TagCircuit: in a tagged circuit every gate has a tag,
--   and wires are taged by the (input) gate they connect to.
data TagGate val tag = TG tag (Gate val tag)
  deriving (Eq, Show, Read, Functor, Traversable, Foldable)
data TagCircuit val tag =  TC { inputs :: [tag]
                              , gates :: [TagGate val tag]}
  deriving (Eq, Show, Read, Functor, Traversable, Foldable)

-- | Circuit: Circuits are assumed to be topologically sorted.
-- Think about it as a tagged circuit where the tag is the index
-- Many operations assume topological order (gate inposition i onlyt
-- takes inputs j<i).
type Circuit val = [Gate val Int]





-- | Error handling
data Err t =
  Ok t | Error String
  deriving (Eq, Show, Read, Functor)

errFromEither :: Either String t -> Err t
errFromEither (Left s) = Error s
errFromEither (Right x) = Ok x

err2Either :: Err t -> Either String t
err2Either (Error s) = Left s
err2Either (Ok x) = Right x


instance Applicative Err where
  pure = Ok
  
  Ok f  <*> m         = fmap f m
  Error s <*> _m      = Error s

  --Lifta2 f (Ok x) (Ok y) = Ok (f x y)
  --liftA2 _ (Error s) _ = Error s
  --liftA2 _ _ (Error s) = Error s
  
  Ok _m1 *> m2      = m2
  Error s  *> _     = Error s
  
instance Monad Err where
  --(>>=) :: Err a -> (a -> Err b) -> Err b
  Error s >>= _ = Error s 
  Ok a >>= f = f a

  --(>>) :: Err a -> Err b -> Err b
  Error s >> _ = Error s
  Ok _ >> eb = eb
  
  --return :: a -> Err a
  return a = Ok a
  
-- | Topological sort: go from a tagged circuit to a Circuit by sorting it and
-- resolvfing the tags.
topSort :: TagCircuit val tag -> Err $ Circuit val
topSort = undefined


renameWiresM :: Monad m => (Int -> m Int) -> Circuit val -> m $ Circuit val
renameWiresM f = mapM (mapM f)

renameWires :: (Int -> Int) -> Circuit val -> Circuit val
renameWires f circ = coerce (renameWiresM @Identity (coerce f) circ)

-- | shiftCircuit: Rename all wires by some   
shiftCircuit :: Int -> Circuit val -> Circuit val
shiftCircuit i = renameWires (\x -> i + x)


-- | Disjoint addition / In parallel
(+++) ::
  TagCircuit val tag
  -> TagCircuit val tag
  ->  TagCircuit val tag
(TC inp1 gates1) +++ (TC inp2 gates2) =
  (TC (inp1 ++ inp2) (gates1 ++ gates2))

-- | Sum with connections: in sequence
seqJoin ::
  (tag2 -> Either tag1 tag2)
  -> [TagGate val tag1]
  ->  [TagGate val tag2]
  ->  [TagGate val (Either tag1 tag2)]
seqJoin conect c1 c2 = map (fmap Left) c1 ++ map (fmap conect) c2


-- | ConectMap: Connection map describes where to connect a list of wires (inputs).
-- `Nothing` means the input remains a global input
type ConectMap tag1 = [Maybe tag1]


updFunc :: Eq k => (k -> v) -> k -> v -> (k -> v)
updFunc f k v x = if x == k then v else f x
-- | evalConectMap: takes a list of inputs and applies a conect map to it producing
-- 1. A list of inputs that are not connected (indicated by `Nothing`) and
-- 2. A function to redirect all wires conected to an input to the new tag
applyConectMap :: Eq tag2 =>
  [tag2] -- ^ list of inputsm 
  -> ConectMap tag1     -- ^ Where to conect those inputs
  -> Err ([tag2],           -- remaining inputs
            tag2 -> Either tag1 tag2)     -- conections from c1 to c2
applyConectMap [] _ = Ok ([], Right)               
applyConectMap (tg2:inpts) (Nothing:cmap) = do
  (inpts', fconect') <- applyConectMap inpts cmap
  Ok (tg2:inpts', fconect')
applyConectMap (tg2:inpts) (Just tg1:cmap) = do
  (inpts', fconect') <- applyConectMap inpts cmap
  Ok (inpts', updFunc fconect' tg2 (Left tg1))
applyConectMap _ _ = Error "Connections of different sizes: Input doesn't match connect map. "
  
-- | Plug in circuits / In series
-- Provide two circuits `c1 c2`, and and a list of `Maybe tag` describing how to plug the inputs of c2
-- into c1 (`Nothing` means that the input remains a global input)
plugCircuits ::
  (Eq tag2) =>
  TagCircuit val tag1 -- ^ c1
  -> TagCircuit val tag2 -- ^ c2
  -> ConectMap tag1 -- ^ Where to plug the inputs to c2.
  -> Err $ TagCircuit val (Either tag1 tag2)
plugCircuits (TC inps1 c1) (TC inps2 c2) cmap = do
  (inps', fconect) <- applyConectMap inps2 cmap
  Ok $ TC (map Left inps1 ++ map Right inps')
    (seqJoin fconect c1 c2)
  

-- ** Circuit evaluation: evaluate the circuit for testing
-- Here is how it works:
-- 1. We start with a map, mapping tags to gates.
-- 2. For each gate, look up each input if its not computed yet,
--    compute that wire first. While doing so marke the current gate as
--    "computing" to avoid loops.

data WireVal val tag =
  WV val                     -- ^ the value in the wire
  | Computing                -- ^ This wire is being computed
  | ToCompute (Gate val tag) -- ^ stores the gate, uncomputed.
  deriving (Show, Read)
type PartialMap val tag = Map.Map tag (WireVal val tag)
type EvalState val tag = State (PartialMap val tag)
type EvalMonad val tag = ExceptT String (EvalState val tag)


commitWire :: WireVal val tag -> Err val
commitWire (WV v) = Ok v
commitWire _ = Error "Wire not computed yet"

-- | Initial Gate map: expresss the circuit ass a map instead of a list.
initCircuitMap ::
  Ord tag =>
  [val]                 -- ^ input values
  -> TagCircuit val tag -- ^ The circuit
  -> Err $ Map.Map tag (WireVal val tag)
initCircuitMap inpts (TC inTag gates) = do
  inptMap <- initInputMap inpts inTag
  gatesMap <- Ok $ initGateMap gates
  Ok $ Map.union inptMap gatesMap

-- | initially all input tags map to their values
initInputMap :: Ord tag => [val] -> [tag] -> Err $ Map.Map tag (WireVal val tag)
initInputMap [] [] = Ok Map.empty
initInputMap (inv:invs) (int:ints) = do
  wmap <- initInputMap invs ints
  Ok $ Map.insert int (WV inv) wmap
initInputMap a b = Error $ "Mismatch on number of inputs when evaluating initInputMap. After popping the same number from each, there are " ++ (show $ length a) ++ " inputs remaining and " ++
  (show $ length b) ++ " tags remaining."

-- | initially all gate tags map to the gate
initGateMap :: Ord tag => [TagGate val tag] -> Map.Map tag (WireVal val tag)
initGateMap gts = foldr addGate Map.empty gts
  where addGate (TG tag gate) = Map.insert tag (ToCompute gate)

-- | eval a list of wires
evalWire :: (Show tag, Ord tag) => tag -> EvalMonad Int tag Int  
evalWire t = do
  wMap <- lift get
  case Map.lookup t wMap of
    Nothing -> throwError $ "Tag not found: " ++ (show t)
    Just (WV v) -> return v
    Just Computing -> throwError $ "Loop starting at " ++ (show t)
    Just (ToCompute gate) -> evalGate gate

evalWires ::  (Show tag, Ord tag) => [tag] -> EvalMonad Int tag [Int]
evalWires ts = mapM evalWire ts

evalGate :: (Show tag, Ord tag) =>
            Gate Int tag
         -> EvalMonad Int tag Int
evalGate (Gconst v) = return v
evalGate (Gadd wls) = do
  values <- evalWires wls
  return $ sum values
evalGate (Gsub x y) = do
  xVal <- evalWire x
  yVal <- evalWire y
  return $ xVal - yVal
evalGate (Gmul wls) = do
  values <- evalWires wls
  return $ product values
evalGate (Gmux b x y) = do
  b' <- evalWire b
  x' <- evalWire x
  y' <- evalWire y
  return $ if b' /= 0 then x' else y'
evalGate (Geq a b) = do
  a <- evalWire a
  b <- evalWire b
  return $ fromEnum $ a == b
evalGate (Ggt a b) = do
  a <- evalWire a
  b <- evalWire b
  return $ fromEnum $ a > b
evalGate (Gge a b) = do
  a <- evalWire a
  b <- evalWire b
  return $ fromEnum $ a >= b
evalGate (Grelay a) = do
  a <- evalWire a
  return $ a

-- | Evaluate: Compute the output of every gate in the circuit.
-- Proceeds by DFS keeping track of visited wires with WireVal

evalCircuit ::
  (Show tag, Ord tag) =>
  TagCircuit Int tag -- ^ The circuit
  -> [Int]           -- ^ Inputs
  -> [tag]           -- ^ Ouputs
  -> Err [Int]
evalCircuit c ins outs = do
  initMap <- initCircuitMap ins c
  let (outs',_) = runState (runExceptT (evalWires outs)) initMap in
    errFromEither outs'


{-
a = TC ["a","b"] [TG "c" $ Gadd ["a","b"]]
b = TC ["a","b"] [TG "c" $ Gadd ["a","b"]]
c = TC ["a","b"] [TG "c" $ Gmul ["a","b"]]

cmap = [Just "c", Nothing]
myC = do
  ab <- plugCircuits a b [Nothing, Nothing]
  abc <- plugCircuits ab c [Just $ Left "c", Just $ Right "c"]
  return abc

evalC = do
  myC' <- myC
  evalCircuit myC' [3,2,1,2] [Right "c"]
-}
