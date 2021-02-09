{-# LANGUAGE TemplateHaskell #-}
module Sparsity.Stutter where

import qualified Data.Map as Map

import Control.Lens (makeLenses, ix, at, to, lens, (^.), (.=), (%=), use, Lens', _1, _2, _3)

import MicroRAM
import MicroRAM.MRAMInterpreter


import Compiler.Errors

import Control.Monad.State

import qualified Data.Set as Set

import Sparsity.Sparsity

--import Debug.Trace

data SparsityState = SparsityState
  { _spSpars :: Sparsity
  , _spLastSeen :: Map.Map InstrKind Int
  , _spCycle :: Int
  , _spProg :: Program () ()
  }
makeLenses ''SparsityState
type SparState m a = StateT SparsityState m a


stutter :: Sparsity ->  Program a b -> Trace mreg -> Hopefully (Trace mreg)
stutter spar prog t = evalStateT (addStuttering t) initState
  where initState = SparsityState spar Map.empty 0 (mapProg mkUnit mkUnit prog)
        mkUnit _ = ()

-- | Given a chunk of trace add the stuttering steps to fit the sparsity.
-- This assumes the trace is "fresh" (i.e. we count the sparsity from 0)
-- Which is the case on new blocks of "private segments"
addStuttering :: Trace mreg -> SparState Hopefully (Trace mreg)
addStuttering t = concat <$> mapM addStutteringStep t 

addStutteringStep :: ExecutionState mreg -> SparState Hopefully [ExecutionState mreg]
addStutteringStep st =
  do prog <- use spProg
     instr <- return $ prog !! (fromEnum $ pc st)
     stutter <- checkSparsity instr -- How long I should stutter
     spCycle %= (+ (stutter + 1))
     return $ (replicate stutter (addStutter st)) ++ [st]
  where addStutter :: ExecutionState mreg -> ExecutionState mreg
        addStutter st = st {advice = Stutter : (advice st)}
 
    
checkSparsity :: Instruction reg wrd -> SparState Hopefully Int
checkSparsity instr = do
  kinds <- return $ instrType instr
  stutters <- mapM sparsity kinds
  return $ maximum stutters
  where sparsity :: InstrKind -> SparState Hopefully Int
        sparsity kind = do
          cyc <- use spCycle
          lastMap <- use spLastSeen
          last <- return $ Map.findWithDefault (-1) kind lastMap
          spLastSeen %=  (Map.insert kind (fromEnum cyc))
          sparsity <- use $ spSpars
          targetSpars <- return $ Map.lookup kind sparsity 
          case targetSpars of
            Nothing  -> return 0
            Just spc -> do
              if sameFunctionalUnit spc last (fromEnum cyc)
                then return $ stutterLength spc cyc
                else return 0
  
        -- | Checks if two instances of the same kind would share the same functional Unit
        -- There is one Functional unit every 'spc' cycles.
        -- Last seen default is -1 so must use `div` 
        sameFunctionalUnit spc last cyc = (last `div` spc) == (cyc `quot` spc)
        -- | Calculates how much we must stutter until we get a new functional unit for this kind.
        stutterLength spc cyc = spc - (cyc `mod` spc)
         

--- TESTING

-- TESTING

fakeState :: MWord -> ExecutionState ()
fakeState n = ExecutionState n Map.empty (0, Map.empty) Set.empty [] False False False 0

testTrace :: Trace ()
testTrace =
  fakeState 0 :
  fakeState 1 :
  fakeState 2 :  --
  fakeState 4 :  
  fakeState 5 :
  fakeState 6 :  -- 
  fakeState 3 :  --
  fakeState 4 :
  fakeState 5 : 
  fakeState 6 :  --
  fakeState 3 :  -- 
  fakeState 4 : 
  fakeState 5 :
  fakeState 6 :  --
  fakeState 3 :  --
  fakeState 4 : 
  fakeState 8 : 
  fakeState 5 :
  fakeState 6 :  --
  fakeState 3 :  --
  fakeState 7 :
  fakeState 8 :  --
  fakeState 3 :  --
  fakeState 7 :
  fakeState 8 : []

testProgram :: Program () ()
testProgram =
  [ Istore W1 (Reg ()) (),
    Iload W2 () (Reg ()),
    Iand () () (Reg ()), 
    Isub () () (Reg ()), 
    Ijmp (Reg ()),
    Icjmp () (Reg ()),
    Iadd () () (Reg ()),
    Isub () () (Reg ()), 
    Ijmp (Reg ()),
    Iand () () (Reg ()),
    Isub () () (Reg ())]

testSparsity :: Hopefully (Trace ())
testSparsity = stutter (Map.fromList [(KmemOp, 4)]) testProgram testTrace

_showTest :: IO ()
_showTest = mapM_ print myTrace
  where myTrace = case testSparsity of
                    Left _ -> []
                    Right x -> x
