module Compiler.Sparcity where

{-
Module      : Opcode Sparcity Analysis
Description : Computes the sparcity of each opcode to aleviate the witness generation 
Maintainer  : santiago@galois.com
Stability   : experimental

For a given opcode `op` the sparcity value is the MINIMUM distance between two 'op' instructions
in the EXECUTION of the program.

For any opcode `op`, sparcity is easy to calculate exactly for straight code
(i.e. inside blocks), call it `spar`. However for jumps and function calls we
must approximate in the following way:

1. We compute the minimum distance of the opcode after a jump/call.
   Call this `Begspar`  (i.e. how early it aperas on a block)
2. We compute the minimum distance of the opcode before a jump/call
   Call this `endSpar` (i.e. how late it appears in a block)
3. `begSpar + endSpar` is the approximation of the sparcity accross jumps

The overall sparcity of `op` is then approximated by `Min (begSpar+endSpar, spar)`.

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

-- * Infinity type
-- We define Infinity types and make them a (somwhat bogus, see below)
-- instance of `Num`. For convenience

-- | Inf: We add infinity to the `Int`s.

data Inf = Infinity | Finite Int
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
  
-- * Sparcity
-- For a given opcode op the sparcity value is the MINIMUM distance
-- between two 'mul' instructions in the EXECUTION of the program.


-- | We approximate sparcity with this triple
data OpSparcity = OpSparcity
  { lastSeen :: Maybe Int -- ^ last location where this instruction appeared
  , spar :: Inf      -- ^ Min distance between `op` in straigh code 
  , begSpar :: Inf   -- ^ Min distance from beggining of blocks
  , endSpar :: Inf   -- ^ Min distance from end of block
  } deriving (Eq, Ord)


-- * Instruction labels.

-- | Works as a label for all instructions of the same "type"
type InstrsLabel = Instruction' () ()

type Sparcity' = Map.Map InstrsLabel OpSparcity -- intermediate
type Sparcity = Map.Map InstrsLabel Int



-- TODO: There probably is a better way to do this 
instrType :: Instruction' r w -> InstrsLabel
instrType inst =
  case inst of
    Iand _ _ _    -> Iand () () ()      
    Ior _ _ _     -> Ior () () ()    
    Ixor _ _ _    -> Ixor () () ()   
    Inot _ _      -> Inot () ()        
    Iadd _ _ _    -> Iadd () () ()   
    Isub _ _ _    -> Isub () () ()   
    Imull _ _ _   -> Imull () () ()  
    Iumulh _ _ _  -> Iumulh () () () 
    Ismulh _ _ _  -> Ismulh () () () 
    Iudiv _ _ _   -> Iudiv () () ()  
    Iumod _ _ _   -> Iumod () () ()  
    Ishl _ _ _    -> Ishl () () ()   
    Ishr _ _ _    -> Ishr () () ()   
    Icmpe _ _     -> Icmpe () ()       
    Icmpa _ _     -> Icmpa () ()       
    Icmpae _ _    -> Icmpae () ()      
    Icmpg _ _     -> Icmpg () ()       
    Icmpge _ _    -> Icmpge () ()      
    Imov _ _      -> Imov () ()        
    Icmov _ _     -> Icmov () ()       
    Ijmp _        -> Ijmp ()             
    Icjmp _       -> Icjmp ()            
    Icnjmp _      -> Icnjmp ()           
    Istore _ _    -> Istore () ()      
    Iload _ _     -> Iload () ()       
    Iread _ _     -> Iread () ()       
    Ianswer _     -> Ianswer ()           


-- * Computing Sparcity

-- ** Sparcity for instructions


setNewEnd loc (OpSparcity (Just lastS) spar begSpar endSpar) =
  OpSparcity (Just lastS) spar begSpar (min endSpar (Finite $ loc - lastS))
setNewEnd loc (OpSparcity Nothing spar begSpar endSpar) =
  OpSparcity Nothing spar begSpar endSpar
  
sparcInstr ::
  (Int, MAInstruction r w)
  -> Sparcity'
  -> Sparcity'
-- For jumps, we don't know where we are going, so we must add the "edge effect"
sparcJump location sparc =
  fmap forgetLastSeen $
  fmap (setNewEnd location) sparc
  where forgetLastSeen (OpSparcity _ spar begSpar endSpar) =
          (OpSparcity Nothing spar begSpar endSpar)
          
updateInstrSparc ::
  Int 
  -> InstrsLabel
  -> (Maybe OpSparcity)
  -> OpSparcity
updateInstrSparc location intrL Nothing =
  OpSparcity (Just location) Infinity (Finite location) Infinity
updateInstrSparc location intrL (Just (OpSparcity Nothing spar begSpar endSpar)) =
  OpSparcity (Just location) spar (Finite location) endSpar
updateInstrSparc location intrL (Just ( OpSparcity (Just lastSeen) spar begSpar endSpar)) =
  OpSparcity (Just location) (min spar (Finite $ location - lastSeen)) begSpar endSpar
  


  

sparcInstr (location, Ijmp _) sparc = sparcJump location sparc
sparcInstr (location, Icjmp _) sparc = sparcJump location sparc
sparcInstr (location, Icnjmp _) sparc = sparcJump location sparc
sparcInstr (location, instr) sparc =
  Map.insert instrLabel (updateInstrSparc location instrLabel instrSparc) sparc
  where instrLabel = instrType instr
        instrSparc = Map.lookup instrLabel sparc
  


-- ** Sparcity for Blocks

-- | Is done in two steps
-- 1. run through the list of instrucitons updateing the sparcity info
-- 2. Set the "End Sparcity" for all instructions in the map
sparcBlock :: NamedBlock r w -> Sparcity'
sparcBlock (NBlock _ instrs) =
  setEndSparc (length instrs) $                 -- second step
  foldr sparcInstr Map.empty $ enumerate instrs -- first step
  where setEndSparc lastLoc sparc = fmap (setNewEnd lastLoc) sparc

-- ** TODO: Functions
-- If we carry the controll flow of functions, we can improve the approximation
-- We merge the sparcity of blocks, only with those adjacent to it (int the right order.
-- We still have "edge effect" at the beggining and end of functions and funciotn
-- calls (which at this point are just `jmp`s.


-- ** Sparcity for Full program
-- We just join the sparc maps by:
-- 1. For each instruction get the min value of each  sparc, endSparc and begSparc
-- 2. Compute min (sparc', endSparc' + begSparc')
sparcity :: MAProgram r w -> Sparcity
sparcity blocks =
  -- read the follwoing backwards
  Map.mapMaybe inf2maybe $       -- remove infinite sparcity (the ones that never appear)
  Map.map addEdgeEffect $     -- return $ min sparc (endSparc + begSparc)
  foldr (Map.unionWith mergeSparc) Map.empty -- get the minimum sparcity values for each instruction
  $ fmap sparcBlock blocks   -- calculate sparcity for each block
  where mergeSparc (OpSparcity ls1 s1 bs1 es1) (OpSparcity ls2 s2 bs2 es2) =
          OpSparcity (max ls1 ls2)
                     (min s1 s2)
                     (min bs1 bs2)
                     (min es1 es2)
        addEdgeEffect (OpSparcity ls s bs es) =
          min s (bs + es)
