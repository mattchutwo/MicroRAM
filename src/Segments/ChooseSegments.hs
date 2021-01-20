{-# LANGUAGE TypeOperators #-}
{-
Module      : Choosing Segments
Description : 
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Segments.ChooseSegments where

-- import MicroRAM
import MicroRAM.MRAMInterpreter
import qualified Data.Map as Map 
-- import Compiler.Errors

import Segments.Segmenting
import Control.Monad.State.Lazy


data TraceChunk reg = TraceChunk {
  chunkSeg :: Int
  , chunkStates :: Trace reg }

data PartialState reg = PartialState {
  remainingTrace :: Trace reg
  , pubBlocks :: [ Int ]
  , priBlocks :: [ Int ]
  , queueSt :: [Trace reg]
  }
type InstrNumber = Word 
type PState reg x = State (PartialState reg) x 
 
chooseSegments :: Int -> Trace reg -> Map.Map Word [Int] -> [Segment reg Word] ->  PState reg [TraceChunk reg]
chooseSegments privSize trace segmentSets segments = undefined

-- | chooses the next segment
chooseSegment :: Int -> ExecutionState reg -> PState reg ()
chooseSegment privSize state = do
  segmentSets <- getSegmentSets
  case Map.lookup (pc state) segmentSets of
    Just (segment : otherSegments) ->
      do allocateQueue
         allocateSegment state segment
    _ -> pushQueue

   where allocateQueue :: PState reg ()
         allocateQueue =
           do queue <- getQueue
              addPrivBlocks (splitPrivBlocks $ reverse queue)
         getQueue :: PState reg [Trace reg]
         getQueue        = do {st <- get; return (queueSt st)}
         addPrivBlocks :: [Int] -> PState reg ()
         addPrivBlocks newBlocks = do
           st <- get
           put (st {priBlocks = priBlocks st ++ newBlocks})
         -----
         splitPrivBlocks = undefined
         allocateSegment state segment = undefined
         pushQueue = undefined
         getSegmentSets = undefined
