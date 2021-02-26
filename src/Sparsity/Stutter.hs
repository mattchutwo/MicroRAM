{-# LANGUAGE TemplateHaskell #-}
module Sparsity.Stutter (stutter, AnnotatedTrace) where

import qualified Data.Map as Map

import Control.Lens (makeLenses, (%=), use)

--import qualified Debug.Trace as Trace(trace)

import MicroRAM
import MicroRAM.MRAMInterpreter


--import Compiler.Errors

import Control.Monad.State

import qualified Data.Set as Set


import Sparsity.Sparsity

--import Debug.Trace

data SparsityState = SparsityState
  { _spSegSize :: Int
  , _spSpars :: Sparsity
  , _spLastSeen :: Map.Map InstrKind Int
  , _spCycle :: Int
  , _spProg :: Program () ()
  }
makeLenses ''SparsityState
type SparState a = State SparsityState a

-- | Trace where every states is annotated with it's curretn pc (execution states have next pc) 
type AnnotatedTrace reg = [(ExecutionState reg, MWord)]

stutter :: Int -> Sparsity ->  Program a b -> AnnotatedTrace mreg -> Trace mreg
stutter segSize spar prog t = evalState (addStuttering t) initState
  where initState = SparsityState segSize spar Map.empty 0 (mapProg mkUnit mkUnit prog)
        mkUnit _ = ()

-- | Given a chunk of trace add the stuttering steps to fit the sparsity.
-- This assumes the trace is "fresh" (i.e. we count the sparsity from 0)
-- Which is the case on new blocks of "private segments"
addStuttering :: AnnotatedTrace mreg -> SparState (Trace mreg)
addStuttering t = concat <$> mapM addStutteringStep t 

addStutteringStep :: (ExecutionState mreg, MWord) -> SparState [ExecutionState mreg]
addStutteringStep (st, currPc) =
  do prog <- use spProg
     instr <- return $ prog !! (fromEnum $ currPc)
     stutter <- checkSparsity instr -- How long I should stutter
     spCycle %= (+ (stutter + 1))
     return $ st : (replicate stutter (addStutter st))
  where addStutter :: ExecutionState mreg -> ExecutionState mreg
        addStutter st = st {advice = Stutter : (advice st)}
 
    
checkSparsity :: Instruction () () -> SparState Int
checkSparsity instr = do
  kinds <- return $ instrType instr
  stutters <- mapM sparsity kinds
  return $ maximum (0:stutters)
  where sparsity :: InstrKind -> SparState Int
        sparsity kind = do
          cyc <- use spCycle
          lastMap <- use spLastSeen
          segSize <- use spSegSize
          last <- return $ Map.findWithDefault (-1) kind lastMap
          sparsity <- use $ spSpars
          targetSpars <- return $ Map.lookup kind sparsity 
          let stutterAmount = case targetSpars of
                              Nothing  -> 0
                              Just spc -> do
                                if sameFunctionalUnit segSize spc last (fromEnum cyc)
                                  then stutterLength segSize spc cyc
                                  else 0
          spLastSeen %= Map.insert kind (fromEnum cyc + stutterAmount)
          return stutterAmount
          
        -- | Checks if two instances of the same kind would share the same functional Unit
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
         

--- TESTING

-- TESTING

fakeState :: MWord -> ExecutionState ()
fakeState n = ExecutionState n Map.empty (0, Map.empty) Set.empty [] False False False 0

testTrace :: AnnotatedTrace ()
testTrace =
  (fakeState 0, 0) :
  (fakeState 1, 0)  :
  (fakeState 2, 1)  : 
  (fakeState 3, 2)  :
  (fakeState 0, 3) :
  (fakeState 1, 0)  :
  (fakeState 2, 1)  : 
  (fakeState 3, 2)  :
  (fakeState 0, 3) :
  (fakeState 1, 0)  :
  (fakeState 2, 1)  : 
  (fakeState 3, 2)  :
  (fakeState 0, 3) :
  (fakeState 1, 0)  :
  (fakeState 2, 1)  : 
  (fakeState 3, 2)  :  []

testProgram :: Program () ()
testProgram =
  [ Istore W1 (Reg ()) (),
    Iload W2 () (Reg ()),
    Iand () () (Reg ()), 
    Ijmp (Reg ())]

testSparsity :: Trace ()
testSparsity = stutter 5 (Map.fromList [(KmemOp, 3)]) testProgram testTrace

_showTest :: IO ()
_showTest = mapM_ print myTrace
  where
    myTrace :: Trace ()
    myTrace = testSparsity
