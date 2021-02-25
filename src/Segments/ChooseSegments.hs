{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-
Module      : Choosing Segments
Description : 
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Segments.ChooseSegments where

import MicroRAM
import MicroRAM.MRAMInterpreter
--import Util.Util

--import qualified Debug.Trace as Debug

import qualified Data.Map as Map 
import qualified Data.Set as Set 
-- import Compiler.Errors

import Segments.Segmenting
import Sparsity.Sparsity
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
  , queueSt :: Trace reg -- Carries the unalocated states backwards. Eventually they go on private chunks.
  , availableSegments :: Map.Map MWord [Int]
  , privLoc :: Int -- The next location of a private segment
  , sparsityPS :: Sparsity
  } deriving Show

type InstrNumber = MWord 
type PState reg x = State (PartialState reg) x 
 
chooseSegments :: Int -> Sparsity -> Trace reg -> Map.Map MWord [Int] -> [Segment reg MWord] ->  [TraceChunk reg]
chooseSegments privSize spar trace segmentSets segments =
  -- create starting state
  let initSt = PartialState {
        nextPc = 0 
        , remainingTrace = (tail trace) -- Drop the first state, which is known.
        , chunksPS = []
        , queueSt = []
        , availableSegments = segmentSets
        , privLoc = (length segments)
        , sparsityPS = spar }   
  -- run the choose statement
      go = whileM_ traceNotEmpty (chooseSegment segments privSize) *> allocateQueue privSize
      finalSt = execState go initSt in 
  -- extract values and return
    (reverse $ chunksPS finalSt)

  where
    -- whilePS :: PState reg Bool -> (PState reg b) -> PState reg ()
    -- whilePS nextM f = do
    --   next <- nextM
    --   if next then f *> whileJust nextM f else return ()

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
chooseSegment :: [Segment reg MWord] -> Int -> PState reg ()
chooseSegment segments privSize = do
  currentPc <- nextPc <$> get
  maybeSegment <- popSegmentIn currentPc
  case maybeSegment of
    Just segment -> do allocateQueue privSize
                       allocateSegment segments segment
    _ -> do execSt <- pullStates 1
            pushQueue (head execSt) -- pop the next state and add it to the queue
         
   where
     allocateSegment :: [Segment reg MWord] -> Int -> PState reg ()
     allocateSegment segments' segmentIndx =
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
     pushQueue :: ExecutionState reg -> PState reg () 
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
{-
allocateQueue :: Int -> PState reg ()
allocateQueue size =
  do queue <- reverse . queueSt <$> get
     modify (\st -> st {queueSt = []})
     currentPrivSegment <- privLoc <$> get
     spar <- sparsityPS <$> get
     --sparseQueue <- return $ stutter sp  
     addPrivBlocks (splitPrivBlocks size currentPrivSegment $ queue)
addPrivBlocks :: [TraceChunk reg] -> PState reg ()
addPrivBlocks newBlocks = do
  st <- get
  put (st {priBlocks = priBlocks st ++ newBlocks})

Keep just until we make sure the above is the right -}

allocateQueue size =  -- TODO: Add sparsity.
  do queue <- queueSt <$> get
     currentPrivSegment <- privLoc <$> get
     newChunks <- return (splitPrivBlocks size currentPrivSegment $ reverse queue)
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
  fakeState 4 :  --
  fakeState 5 :
  fakeState 6 :   
  fakeState 3 :  --
  fakeState 4 :  --
  fakeState 5 : 
  fakeState 6 :  
  fakeState 3 :  -- 
  fakeState 4 : 
  fakeState 5 :
  fakeState 6 :  --
  fakeState 3 :  
  fakeState 4 : 
  fakeState 8 :  --
  fakeState 5 :
  fakeState 6 :  
  fakeState 3 :  --
  fakeState 7 :  --
  fakeState 8 :  
  fakeState 3 :  --
  fakeState 7 :
  fakeState 8 : [] --

pcConstraint n = [PcConst n]
segment0 =
  Segment
  [ Iand () () (Reg ()), 
    Isub () () (Reg ()), 
    Ijmp (Reg ()) ]
    (pcConstraint 0)
    3
    []
    False
    False
segment1 =
  Segment
  [Icjmp () (Const 0)]
  (pcConstraint 3)
  1
  []
  False 
  False
segment2 =
  Segment
  [Iadd () () (Const 0),
    Isub () () (Const 0), 
    Ijmp (Const 3)]
  (pcConstraint 4)
  3
  []
  False
  False
segment3 =
  Segment
  [Iand () () (Reg ()),
    Isub () () (Const 0)]
  (pcConstraint 7)
  2
  []
  False
  False

testSegments' :: [Segment () MWord]
testSegments' =
  [segment0,
    segment1,
    segment2,
    segment2,
    segment3]


testSegmentSets :: Map.Map MWord [Int]
testSegmentSets = Map.fromList
  [(0, [0]),
   (3, [1]),
   (4, [2,3]),
   (7, [4])
  ]

_testchunks :: [TraceChunk ()]
_testchunks = chooseSegments 3 Map.empty testTrace testSegmentSets testSegments'

