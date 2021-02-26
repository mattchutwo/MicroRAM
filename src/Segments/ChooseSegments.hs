{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-
Module      : Choosing Segments
Description : 
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Segments.ChooseSegments where
import qualified Debug.Trace as Trace(trace)

import MicroRAM
import MicroRAM.MRAMInterpreter

import qualified Data.Map as Map 
import qualified Data.Set as Set 

import Segments.Segmenting
import Sparsity.Sparsity
import Sparsity.Stutter
import Control.Monad.State.Lazy

data TraceChunk reg = TraceChunk {
  chunkSeg :: Int
  , chunkStates :: Trace reg }
  --deriving Eq

instance Show (TraceChunk reg) where
  show tc = "# TraceChunk: Indx = " ++ (show $ chunkSeg tc) ++ ". ChunkStates = " ++ (show $ map pc (chunkStates tc))++ ". " 
data PartialState reg = PartialState {
  nextPc :: MWord 
  , remainingTrace :: Trace reg
  , chunksPS :: [TraceChunk reg]
  , queueSt :: AnnotatedTrace reg -- ^ Carries the unalocated states backwards, Eventually they go on private chunks.  They are annotated by the current pc (stuttering needs to know current pc, states have next pc)
  , availableSegments :: Map.Map MWord [Int]
  , privLoc :: Int -- The next location of a private segment
  , sparsityPS :: Sparsity
  , progPS :: Program reg MWord
  } deriving Show

type InstrNumber = MWord 
type PState reg x = State (PartialState reg) x 
 
chooseSegments :: Int -> Sparsity -> Program reg MWord -> Trace reg -> Map.Map MWord [Int] -> [Segment reg MWord] ->  [TraceChunk reg]
chooseSegments privSize spar prog trace segmentSets segments =
  -- create starting state
  let initSt = PartialState {
        nextPc = 0 
        , remainingTrace = (tail trace) -- Drop the first state, which is known.
        , chunksPS = []
        , queueSt = []
        , availableSegments = segmentSets
        , privLoc = (length segments)
        , sparsityPS = spar
        , progPS = prog }   
  -- run the choose statement
      go = Trace.trace ("ChooseSEgments go") $ whileM_ traceNotEmpty (chooseSegment segments privSize) *> allocateQueue privSize
      finalSt = execState go initSt in 
  -- extract values and return
    Trace.trace ("ChooseSEgments End") $ (reverse $ chunksPS finalSt)

  where
    traceNotEmpty :: PState reg Bool
    traceNotEmpty = do
      trace' <- remainingTrace <$> get
      return $ (not . null) trace'
whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
  where go = do
          x <- p
          if x then f >> go else return ()
    

        
showESt :: ExecutionState mreg -> String
showESt es = "EState at " ++ (show $ pc es) ++ ". "

showQueue
  :: (Foldable t, Functor t) => t (ExecutionState mreg) -> String
showQueue q = "[" ++ (concat $ showESt <$> q) ++ "]"
   
-- | chooses the next segment
chooseSegment ::  [Segment reg MWord] -> Int -> PState reg ()
chooseSegment segments privSize = do
  Trace.trace ("Choosing One segment1") $ return ()
  currentPc <- nextPc <$> get
  Trace.trace ("Choosing One segment2") $ return ()
  maybeSegment <- Trace.trace ("popSegment") $ popSegmentIn currentPc
  Trace.trace ("Choosing One segment3") $ return ()
  Trace.trace ("Choosing One segment4") $ case Trace.trace ("Case") $ maybeSegment of
    Just segment -> do
      Trace.trace ("further") $ return ()
      allocateQueue privSize
      allocateSegment segments segment
    _ -> do Trace.trace ("Pushqueue") $  return ()
            execSt <- pullStates 1
            pushQueue (head execSt, currentPc) -- pop the next state and add it to the queue
   where
     allocateSegment :: [Segment reg MWord] -> Int -> PState reg ()
     allocateSegment segments' segmentIndx =
       Trace.trace ("allocateSegment") $
       let segment = segments' !! segmentIndx
           len = segLen segment in
         do statesTail <- pullStates len
            newChunk <- return $ TraceChunk segmentIndx (statesTail)
            addChunks [newChunk] 
     
     -- Gets the next n states, update pc to match the last popped state
     pullStates :: Int -> PState reg [ExecutionState reg]
     pullStates n = do
       remTrace <- remainingTrace <$> get
       states <- return $ (take n) remTrace
       -- Update the list AND the pc of the last popped state
       modify (\st -> st {remainingTrace = drop n $ remTrace, nextPc = pc $ last states})
       return $ states
     pushQueue :: (ExecutionState reg, MWord) -> PState reg () 
     pushQueue state' = do
       st <- get
       put $ st {queueSt = state' : queueSt st}
     popSegmentIn :: MWord -> PState reg (Maybe Int) 
     popSegmentIn instrPc = do
       st <- get
       avalStates <- return $ availableSegments st
       case Map.lookup instrPc avalStates of
         Just (execSt' : others) ->
           do
             _ <- put $ st {availableSegments = Map.insert instrPc others avalStates}
             return $ Just execSt'
         _ -> return Nothing
allocateQueue :: Int -> PState reg ()
allocateQueue size =
  do queue <- reverse . queueSt <$> get
     spar <- sparsityPS <$> get
     prog <- progPS <$> get
     let sparseTrace = stutter size spar prog queue
     currentPrivSegment <- privLoc <$> get
     newChunks <- return (splitPrivBlocks size currentPrivSegment sparseTrace)
     modify (\st -> st {queueSt = [], privLoc = currentPrivSegment + length newChunks})
     addChunks newChunks
-- Chunks are added backwards!
addChunks :: [TraceChunk reg] -> PState reg ()
addChunks newBlocks = modify (\st -> st {chunksPS = (reverse newBlocks) ++ chunksPS st})


splitPrivBlocks :: Int -> Int -> Trace reg -> [TraceChunk reg]
splitPrivBlocks size privLoc' privTrace =
  let splitTrace = padLast size $ splitEvery size privTrace in
    map (\(states, seg) ->  TraceChunk seg states) (zip splitTrace [privLoc' ..])
  where
    -- Make sure all chunks are the same length, padding the last one with stutters
    padLast :: Int -> [[ExecutionState reg]] -> [[ExecutionState reg]]
    padLast _size = modifyLast (padChunk size)
    padChunk :: Int -> [ExecutionState reg] -> [ExecutionState reg]
    padChunk size' ls =
      let lastSt = last ls
          stutterSt = lastSt {advice = [Stutter] }
          pad = replicate (size' - length ls) stutterSt  in
        (init ls) ++ lastSt : pad
    modifyLast :: (a -> a) -> [a] -> [a]
    modifyLast _ [] = []
    modifyLast f [a] = [f a]
    modifyLast f (x:xs) = x:(modifyLast f xs)
     

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

-------
-- TESTING

fakeState :: MWord -> ExecutionState ()
fakeState n = ExecutionState n Map.empty (0, Map.empty) Set.empty [] False False False 0

testTrace :: Trace ()
testTrace =
  fakeState 0 :
  fakeState 1 :
  fakeState 2 : 
  fakeState 3 :
  fakeState 0 :
  fakeState 1 : 
  fakeState 2 : 
  fakeState 3 :
  fakeState 0 :
  fakeState 1 :
  fakeState 2 : 
  fakeState 3 :
  fakeState 0 :
  fakeState 1 : 
  fakeState 2 : 
  fakeState 3 :
  fakeState 0 :
  fakeState 1 : 
  fakeState 2 : 
  fakeState 3 : [] --

pcConstraint n = [PcConst n]
segment0 =
  Segment
  [ Istore W1 (Reg ()) (),
    Iload W2 () (Reg ())]
    (pcConstraint 0)
    2
    []
    False
    False
segment1 =
  Segment
  [ Iand () () (Reg ()), 
    Ijmp (Reg ())]
  (pcConstraint 2)
  2
  []
  False 
  False

testSegments' :: [Segment () MWord]
testSegments' =
  [segment0,
    segment1, 
    segment1]


testSegmentSets :: Map.Map MWord [Int]
testSegmentSets = Map.fromList
  [(0, [0]),
   (2, [1,2])]

testProg = concat $ segIntrs <$> testSegments'

_testchunks :: [TraceChunk ()]
_testchunks = chooseSegments testPrivSize testSparsity testProg testTrace testSegmentSets testSegments'
  where testSparsity = (Map.fromList [(KmemOp, 2)])
        testPrivSize = 4

test = mapM_ (putStrLn . show) _testchunks
