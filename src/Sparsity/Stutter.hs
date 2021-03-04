{-# LANGUAGE TemplateHaskell #-}
module Sparsity.Stutter (stutter, AnnotatedTrace) where

import qualified Data.Map as Map

import Control.Lens (makeLenses, (%=), use)

import MicroRAM
import MicroRAM.MRAMInterpreter

import Control.Monad.State

import qualified Data.Set as Set

import qualified Debug.Trace as Trace (trace)

import Sparsity.Sparsity

data SparsityState = SparsityState
  { _spSegSize :: Int
  , _spSpars :: Sparsity
  , _spLastSeen :: Map.Map InstrKind Int
  , _spCycle :: Int      -- ^ count of last cycle
  , _spProg :: Program () ()
  }
makeLenses ''SparsityState
type SparState a = State SparsityState a

-- | Trace where every states is annotated with it's curretn pc (execution states have next pc) 
type AnnotatedTrace reg = [(ExecutionState reg, MWord)] -- TODO: Remove

stutter :: Int -> Sparsity ->  Program a b -> AnnotatedTrace mreg -> Trace mreg
stutter _ _ _ [] = []
stutter segSize spar prog t =
  evalState (addStuttering simplTrace) initState
  where initState = SparsityState segSize spar initLastSeen 0 (mapProg mkUnit mkUnit prog)
        -- The first step never stutters, but must be recorded as last seen.
        firstInstr = prog !! fromEnum (snd $ head t)
        initLastSeen = Map.fromList $ (\kind -> (kind, 0)) <$> (instrType firstInstr)
        simplTrace = fst <$> t
        mkUnit _ = ()

-- | Given a chunk of trace add the stuttering steps to fit the sparsity.
-- This assumes the trace is "fresh" (i.e. we count the sparsity from 0)
-- Which is the case on new blocks of "private segments"
addStuttering :: Trace mreg -> SparState (Trace mreg)
addStuttering t = concat <$> mapM addStutteringStep t 

-- | Note: this checks if the NEXT instruction can safely execute.
-- iIf it can't, then the current state stutters until it is safe to proceed
addStutteringStep :: ExecutionState mreg -> SparState [ExecutionState mreg]
addStutteringStep st =
  do prog <- use spProg
     let nextPc = pc st -- ^ this point to the next instruction
     instr <- return $ prog !! (fromEnum $ nextPc)
     stutterAmmount <- checkSparsity instr -- How long I should stutter
     spCycle %= (+ (stutterAmmount + 1))
     return $ st : (replicate stutterAmmount (addStutter st)) -- add the current state and stutters until it's safe to perform the next instruction.
  where addStutter :: ExecutionState mreg -> ExecutionState mreg
        addStutter st = st {advice = [Stutter]} -- Steps that stutter only stutter (i.e. remove all other advice)
 
    
checkSparsity :: Instruction () () -> SparState Int
checkSparsity instr = do
  let kinds = instrType instr
  stutters <- mapM sparsity kinds
  return $ maximum (0:stutters)
  where sparsity :: InstrKind -> SparState Int
        sparsity kind = do
          nextCyc <- (+ 1) <$> use spCycle -- We check the viability of the NEXT step
          lastMap <- use spLastSeen
          segSize <- use spSegSize
          last <- return $ Map.findWithDefault (-1) kind lastMap
          sparsity <- use $ spSpars
          targetSpars <- return $ Map.lookup kind sparsity 
          let stutterAmount = case targetSpars of
                              Nothing  -> 0
                              Just spc -> do
                                if sameFunctionalUnit segSize spc last (fromEnum nextCyc)
                                  then stutterLength segSize spc (nextCyc)
                                  else 0
          spLastSeen %= Map.insert kind (fromEnum nextCyc + stutterAmount) -- Notice we are storing the next instruction, assuming it will come right after.
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
fakeState n = ExecutionState n Map.empty (0, Map.empty) Set.empty [] False False False 0

testTrace :: AnnotatedTrace ()
testTrace =
  (fakeState 0, 1) :
  (fakeState 1, 0)  :
  (fakeState 2,  1)  : 
  (fakeState 3,  2)  :
  (fakeState 0,  3) :
  (fakeState 1,  4)  :
  (fakeState 2,  5)  : 
  (fakeState 3,  6)  :
  (fakeState 0,  7) :
  (fakeState 1,  8)  :
  (fakeState 2,  9)  : 
  (fakeState 3, 10)  :
  (fakeState 0, 11) :
  (fakeState 1, 12)  :
  (fakeState 2, 13)  : 
  (fakeState 3, 14)  :  []

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
