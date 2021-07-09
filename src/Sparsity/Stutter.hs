{-# LANGUAGE TemplateHaskell #-}
module Sparsity.Stutter (stutter) where

import qualified Data.Map as Map

import Control.Lens (makeLenses, (%=), use)

import MicroRAM
import MicroRAM.MRAMInterpreter

import Control.Monad.State

import qualified Data.Set as Set

--import qualified Debug.Trace as Trace (trace)

import Sparsity.Sparsity

data SparsityState = SparsityState
  { _spSegSize :: Int
  , _spSpars :: Sparsity
  , _spLastSeen :: Map.Map InstrKind Int
  , _spCycle :: Int      -- ^ Most recent cycle
  , _spProg :: Program () ()
  }
makeLenses ''SparsityState
type SparState a = State SparsityState a

stutter :: Int -> Sparsity ->  Program a b -> Trace mreg -> Trace mreg
stutter _ _ _ [] = []
stutter segSize spar prog t =
  evalState (addStuttering t) initState
  where initState = SparsityState segSize spar Map.empty 0 (mapProg (const ()) (const ()) prog)
  
-- | Given a chunk of trace add the stuttering steps to fit the sparsity.
-- Note: We realy on the fact that the first state never stutters. That state will be dropped.
addStuttering :: Trace mreg -> SparState (Trace mreg)
addStuttering t = concat <$> mapM addStutteringStep t 

-- | Note: this checks if the NEXT instruction can safely execute.
-- If it can't, then we stutter in the current state until it is safe to proceed
addStutteringStep :: ExecutionState mreg -> SparState [ExecutionState mreg]
addStutteringStep st =
  do prog <- use spProg
     let nextInstrIndx = pc st -- this point to the next instruction
     let nextInstr = prog !! fromEnum nextInstrIndx
     stutterAmmount <- checkSparsity nextInstr -- How long I should stutter
     spCycle %= (+ (stutterAmmount + 1))
     return $ st : replicate stutterAmmount (addStutter st) -- add the current state and stutters until it's safe to perform the next instruction.
  where addStutter :: ExecutionState mreg -> ExecutionState mreg
        addStutter st = st {advice = [Stutter]} -- Steps that stutter, only stutter (i.e. remove all other advice)
 
    
checkSparsity :: Instruction () () -> SparState Int
checkSparsity instr = do
  let kinds = instrType instr
  stutters <- mapM doSparsity kinds
  return $ maximum (0:stutters)
  where doSparsity :: InstrKind -> SparState Int
        doSparsity kind = do
          cyc <- use spCycle -- We check the viability of the NEXT step
          lastMap <- use spLastSeen
          segSize <- use spSegSize
          let lastSeen = Map.findWithDefault (-1) kind lastMap
          spar <- use spSpars
          let targetSpars = Map.lookup kind spar 
          let stutterAmount = case targetSpars of
                              Nothing  -> 0
                              Just spc ->
                                if sameFunctionalUnit segSize spc lastSeen (fromEnum cyc) then
                                  stutterLength segSize spc cyc
                                else
                                  0
          spLastSeen %= Map.insert kind (fromEnum cyc + stutterAmount) -- Notice we are storing the next instruction, assuming it will come right after.
          return stutterAmount
          
        -- | Checks if the current cycle shares a functional unit with the last seen use of that function.
        -- There is one Functional unit every 'spc' cycles. But it start counting
        -- at the beggingin of each segment.
        -- Last seen default is -1 so must use `div` 
        sameFunctionalUnit segSize spc last cyc =
          inTheSameSegment && inTheSameUnit
          where
            inTheSameSegment = inTheSame segSize last cyc
            inTheSameUnit = inTheSame spc (last `mod` segSize) (cyc `mod` segSize) -- ^ modulo segSize because all units start fresh at the beg. of each segment.
            inTheSame size a b = (a `div` size) == (b `quot` size) 
        -- | Calculates how much we must stutter until we get a new functional unit for this kind.
        -- Explanation in words: Everything is fresh at the begginig og the segment so use the
        -- cycle modulo the `segSize`. Then take modulo `spc` to see how many slots have been steped
        -- within the current functional unit. Substract that from `spc` to see how many steps until the
        -- next functional unit. 
        stutterLength segSize spc cyc = spc - ((cyc `mod` segSize) `mod` spc)
        -- 2 - ()

--- TESTING

-- TESTING

fakeState :: MWord -> ExecutionState ()
fakeState n = ExecutionState n Map.empty Nothing (0, Map.empty, Nothing) Set.empty [] False False 0

testTrace :: Trace ()
testTrace =
  (fakeState 0) :
  (fakeState 1)  :
  (fakeState 2)  : 
  (fakeState 3)  :
  (fakeState 0) :
  (fakeState 1)  :
  (fakeState 2)  : 
  (fakeState 3)  :
  (fakeState 0) :
  (fakeState 1)  :
  (fakeState 2)  : 
  (fakeState 3)  :
  (fakeState 0) :
  (fakeState 1)  :
  (fakeState 2)  : 
  (fakeState 3)  :  []

testProgram :: Program () ()
testProgram =
  [ Istore W1 (Reg ()) (),
    Iload W2 () (Reg ()),
    Iand () () (Reg ()), 
    Ijmp (Reg ())]

testSparsity :: Trace ()
testSparsity = stutter 10 (Map.fromList [(KmemOp, 3)]) testProgram (tail testTrace)

_showTest :: IO ()
_showTest = mapM_ print myTrace
  where
    myTrace :: Trace ()
    myTrace = testSparsity
