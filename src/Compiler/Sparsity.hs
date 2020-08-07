{-# LANGUAGE OverloadedStrings #-}
module Compiler.Sparsity (sparsity, Sparsity, InstrKind) where

{-
Module      : Opcode Sparsity Analysis
Description : Computes the sparsity of each opcode to aleviate the witness generation 
Maintainer  : santiago@galois.com
Stability   : experimental

For a given opcode `op` the sparsity value is the MINIMUM distance between two 'op' instructions
in the EXECUTION of the program.

For any opcode `op`, sparsity is easy to calculate exactly for straight code
(i.e. inside blocks), call it `spar`. However for jumps and function calls we
must approximate in the following way:

1. We compute the minimum distance of the opcode after a jump/call.
   Call this `Begspar`  (i.e. how early it aperas on a block)
2. We compute the minimum distance of the opcode before a jump/call
   Call this `endSpar` (i.e. how late it appears in a block)
3. `begSpar + endSpar` is the approximation of the sparsity accross jumps

The overall sparsity of `op` is then approximated by `Min (begSpar+endSpar, spar)`.

TODO: We can improve the approximation of a accross jumps by following the
controll flow graph (in development). However the program can allways make
indirect calls which make some jumps unknowable at compile time.

NOTE: we are assuming here that we are compiling a `complete` program.
But supporting separate compilation is easy:

1. Produce the intermediate tuple `(spar,begSpar,endSpar)` for each compilation unit
   and each library.
2. Then "join" the tuples for the same opcode, producing a triple `(spar',begSpar',endSpar')`
   for the entire program.
3. Return `Min (begSpar' + endSpar', spar')`

-}

import qualified Data.Map as Map

import Util.Util
import MicroRAM.MicroRAM

--  Temp
import Compiler.IRs
import Control.Monad


-- * Infinity type
-- We define Infinity types and make them a (somwhat bogus, see below)
-- instance of `Num`. For convenience

-- | Inf: We add infinity to the `Int`s.

data Inf = Finite Int | Infinity
  deriving (Eq, Ord, Show, Read)

inf2maybe :: Inf -> Maybe Int
inf2maybe (Finite n) = Just n
inf2maybe Infinity = Nothing



-- | In this instance, `(-)` is boggus:
-- we chose to define it as (x - infinity = infinity) 

liftBinop :: (Int -> Int -> Int) -> Inf -> Inf -> Inf
liftBinop _ Infinity _ = Infinity
liftBinop _ _ Infinity = Infinity
liftBinop bop (Finite x) (Finite y) = Finite $ x `bop` y

liftUnop :: (Int -> Int) -> Inf -> Inf
liftUnop _ Infinity = Infinity
liftUnop uop (Finite x) = Finite $ uop x

instance Num Inf where
  (+) = liftBinop (+)
  (*) = liftBinop (*)
  (-) = liftBinop (-)
  
  abs = liftUnop abs
  signum = liftUnop signum
  negate = liftUnop negate

  fromInteger = Finite . fromInteger
  
-- * Sparsity
-- For a given opcode op the sparsity value is the MINIMUM distance
-- between two 'mul' instructions in the EXECUTION of the program.


-- | We approximate sparsity with this triple
data OpSparsity = OpSparsity
  { lastSeen :: Maybe Int -- ^ last location where this instruction appeared
  , spar :: Inf      -- ^ Min distance between `op` in straigh code 
  , begSpar :: Inf   -- ^ Min distance from beggining of blocks
  , endSpar :: Inf   -- ^ Min distance from end of block
  } deriving (Eq, Ord, Show)


-- * Instruction labels.

-- | Works as a label for all instructions of the same "type"
data InstrKind =
  Kand   
  | Kor    
  | Kxor   
  | Knot   
  | Kadd   
  | Ksub   
  | Kmull  
  | Kumulh 
  | Ksmulh 
  | Kudiv  
  | Kumod  
  | Kshl   
  | Kshr   
  | Kcmpe  
  | Kcmpa  
  | Kcmpae 
  | Kcmpg  
  | Kcmpge 
  | Kmov   
  | Kcmov  
  | Kjmp   
  | Kcjmp  
  | Kcnjmp 
  | Kstore 
  | Kload  
  | Kread  
  | Kanswer
  -- Generic classes
  | KmemOp  -- Memory operations
  | Kalu    -- ALU operations
  | Kjumps  -- Aljump operations
 deriving (Eq, Ord, Read, Show)

type Sparsity' = Map.Map InstrKind OpSparsity -- intermediate
type Sparsity = Map.Map InstrKind Int



-- TODO: There probably is a better way to dos this 
instrType :: Instruction' r w -> [InstrKind]
instrType inst =
  case inst of
    Iand _ _ _    -> [Kand   , Kalu]
    Ior _ _ _     -> [Kor    , Kalu]
    Ixor _ _ _    -> [Kxor   , Kalu]
    Inot _ _      -> [Knot   , Kalu]     
    Iadd _ _ _    -> [Kadd   , Kalu]
    Isub _ _ _    -> [Ksub   , Kalu]
    Imull _ _ _   -> [Kmull  , Kalu]
    Iumulh _ _ _  -> [Kumulh , Kalu]
    Ismulh _ _ _  -> [Ksmulh , Kalu]
    Iudiv _ _ _   -> [Kudiv  , Kalu]
    Iumod _ _ _   -> [Kumod  , Kalu]
    Ishl _ _ _    -> [Kshl   , Kalu]
    Ishr _ _ _    -> [Kshr   , Kalu]
    Icmpe _ _     -> [Kcmpe  , Kalu]     
    Icmpa _ _     -> [Kcmpa  , Kalu]     
    Icmpae _ _    -> [Kcmpae , Kalu]     
    Icmpg _ _     -> [Kcmpg  , Kalu]     
    Icmpge _ _    -> [Kcmpge , Kalu]     
    Imov _ _      -> [Kmov   , Kalu]     
    Icmov _ _     -> [Kcmov  , Kalu]     
    Ijmp _        -> [Kjmp  , Kjumps]          
    Icjmp _       -> [Kcjmp , Kjumps]          
    Icnjmp _      -> [Kcnjmp, Kjumps]          
    Istore _ _    -> [Kstore , KmemOp]     
    Iload _ _     -> [Kload  , KmemOp]     
    Iread _ _     -> [Kread  ]     
    Ianswer _     -> [Kanswer]           


-- * Computing Sparsity

-- ** Sparsity for instructions


setNewEnd loc (OpSparsity (Just lastS) spar begSpar endSpar) =
  OpSparsity (Just lastS) spar begSpar (min endSpar (Finite $ loc - lastS))
setNewEnd loc (OpSparsity Nothing spar begSpar endSpar) =
  OpSparsity Nothing spar begSpar endSpar
  
sparsJump location spars =
  fmap forgetLastSeen $
  fmap (setNewEnd location) spars
  where forgetLastSeen (OpSparsity _ spar begSpar endSpar) =
          (OpSparsity Nothing spar begSpar endSpar)
          
updateInstrSpars ::
  Int 
  -> InstrKind
  -> (Maybe OpSparsity)
  -> OpSparsity
updateInstrSpars location intrL Nothing =
  OpSparsity (Just location) Infinity (Finite location) Infinity
updateInstrSpars location intrL (Just (OpSparsity Nothing spar begSpar endSpar)) =
  OpSparsity (Just location) spar (min begSpar $ Finite location) endSpar
updateInstrSpars location intrL (Just ( OpSparsity (Just lastSeen) spar begSpar endSpar)) =
  OpSparsity (Just location) (min spar (Finite $ location - lastSeen)) begSpar endSpar
  


  

sparsInstr ::
  Sparsity'
  -> (Int, MAInstruction r w)
  -> Sparsity'
-- For jumps, we don't know where we are going, so we must add the "edge effect"
sparsInstr spars (location, Ijmp _) = sparsJump location spars
sparsInstr spars (location, Icjmp _) = sparsJump location spars
sparsInstr spars (location, Icnjmp _) = sparsJump location spars
sparsInstr spars (location, instr) =
  foldr updateKindSparc spars $ instrType instr
  where updateKindSparc :: InstrKind -> Sparsity' -> Sparsity'
        updateKindSparc = undefined
    
--  Map.insert instrLabel (updateInstrSpars location instrLabel instrSpars) spars
--  where 
--        instrSpars = Map.lookup instrLabel spars
  


-- ** Sparsity for Blocks

-- | Is done in two steps
-- 1. run through the list of instrucitons updateing the sparsity info
-- 2. Set the "End Sparsity" for all instructions in the map
sparsBlock :: NamedBlock r w -> Sparsity'
sparsBlock (NBlock _ instrs) =
  setEndSpars (length instrs) $                 -- second step
  foldl sparsInstr Map.empty $ enumerate instrs -- first step
  where setEndSpars lastLoc spars = fmap (setNewEnd lastLoc) spars

-- ** TODO: Functions
-- If we carry the controll flow of functions, we can improve the approximation
-- We merge the sparsity of blocks, only with those adjacent to it (int the right order.
-- We still have "edge effect" at the beggining and end of functions and funciotn
-- calls (which at this point are just `jmp`s.


-- ** Sparsity for Full program
-- We just join the spars maps by:
-- 1. For each instruction get the min value of each  spars, endSpars and begSpars
-- 2. Compute min (spars', endSpars' + begSpars')
sparsity :: MAProgram r w -> Sparsity
sparsity blocks =
  -- read the follwoing backwards
  Map.mapMaybe inf2maybe $       -- remove infinite sparsity (the ones that never appear)
  Map.map addEdgeEffect $     -- return $ min spars (endSpars + begSpars)
  foldr (Map.unionWith mergeSpars) Map.empty -- get the minimum sparsity values for each instruction
  $ fmap sparsBlock blocks   -- calculate sparsity for each block
  where mergeSpars (OpSparsity ls1 s1 bs1 es1) (OpSparsity ls2 s2 bs2 es2) =
          OpSparsity (max ls1 ls2)
                     (min s1 s2)
                     (min bs1 bs2)
                     (min es1 es2)
        addEdgeEffect (OpSparsity ls s bs es) =
          min s (bs + es)
