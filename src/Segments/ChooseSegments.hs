{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-
Module      : Choosing Segments
Description : 
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Segments.ChooseSegments where

import qualified Debug.Trace as T (trace, traceShow)
import MicroRAM
import MicroRAM.MRAMInterpreter

import qualified Data.Map as Map 
import qualified Data.Set as Set 
import qualified Data.Vector as V (Vector, (!), fromList, ifoldl)

import Segments.Segmenting
import Sparsity.Sparsity
import Sparsity.Stutter
import Control.Monad.State.Lazy

import Compiler.Errors

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
  , queueSt :: Trace reg -- ^ Carries a list of visited but unalocated states (backwards) including the inital state.
  , availableSegments :: Map.Map MWord [Int]
  , successorSegs :: [Int] -- successors to the current segment
  , stToNetwork :: Bool -- If the current segment goes to network. 
  , privLoc :: Int -- The next location of a private segment
  , sparsityPS :: Sparsity
  , progPS :: Program reg MWord
  , usedSegments :: Set.Set Int
  } deriving Show

type InstrNumber = MWord 
type PState reg x = StateT (PartialState reg) Hopefully x 
 
chooseSegments :: Show reg => Int -> Sparsity -> Program reg MWord -> Trace reg -> V.Vector (Segment reg MWord) ->  Hopefully [TraceChunk reg]
chooseSegments privSize spar prog trace segments = do
  -- create starting state
  let initSt = PartialState {
        nextPc = 0 
        , remainingTrace = tail trace -- Drop the initial state
        , chunksPS = []
        , queueSt = [head trace] -- initial state.
        , availableSegments = segmentSets
        , successorSegs = []
        , stToNetwork = True
        , privLoc = length segments
        , sparsityPS = spar
        , progPS = prog
        , usedSegments = Set.empty}   
  -- run the choose statement
  let go = whileM_ traceNotEmpty (chooseSegment segments privSize) *> allocateQueue privSize
  finalSt <- execStateT go initSt 
  -- extract values and return
  return (reverse $ chunksPS finalSt)

  where
    traceNotEmpty :: PState reg Bool
    traceNotEmpty = do
      trace' <- remainingTrace <$> get
      return $ (not . null) trace'
    -- | Maps pc to the index of segments that start with that pc
    -- but only if the segment comes from network
    segmentSets :: Map.Map MWord [Int]
    segmentSets = segMap -- T.trace ("Map of pc -> segment: " ++ show segMap) segMap
      where segMap = V.ifoldl addSegment Map.empty segments
    addSegment :: Map.Map MWord [Int] -> Int -> Segment reg wrd -> Map.Map MWord [Int] 
    addSegment sets indx seg =
      if fromNetwork seg
      then addToMap sets (segPc seg) indx
      else sets
    addToMap :: Map.Map MWord [Int] -> Maybe MWord -> Int -> Map.Map MWord [Int] 
    addToMap mp mk a =
      case mk of
        Nothing -> mp
        Just k -> let currentValue = Map.lookup k mp in
                    flip (Map.insert k) mp $ case currentValue of
                                               Just ls -> a:ls
                                               Nothing -> [a]
    segPc seg = getPcFromConstraint (constraints seg)
    getPcFromConstraint :: [Constraints] -> Maybe MWord 
    getPcFromConstraint constrs =
      case constrs of
        (PcConst pcW):_ -> Just pcW
        --_: ls -> getPcFromConstraint ls -- ^ GCC is smart enought to notice this is redundant ADD IT if more constraints are added
        [] -> Nothing
                    
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
chooseSegment :: Show reg => V.Vector (Segment reg MWord) -> Int -> PState reg ()
chooseSegment segments privSize = do
  currentPc <- nextPc <$> get
  avalStates <- availableSegments <$> get
  succs <- successorSegs <$> get
  toNet <- stToNetwork <$> get
  let networkSuccs = if toNet then Map.findWithDefault [] currentPc avalStates else []
  let possibleNextSegments = succs ++ networkSuccs
  checkedNextSegments <- filterM (checkSegment False segments) possibleNextSegments
  when (null checkedNextSegments) $ do
    _ <- filterM (checkSegment True segments) possibleNextSegments
    return ()
  case checkedNextSegments of -- T.trace ("Pc :" ++ show currentPc ++ ". Possible next: " ++ show checkedNextSegments ++ "\n\tUnfiltered: " ++ show possibleNextSegments) checkedNextSegments of 
    segment:_ -> do
      queue <- queueSt <$> get
      -- allocates the current queue in private pc segments (if there is more than just the last state).  
      when (length queue >1) $ allocateQueue privSize
      -- allocates states to use the public pc segment. Returns the last state
      queueInitSt <- allocateSegment segments segment
      let newSeg = segments V.! segment
      modify (\st -> st {queueSt = [queueInitSt] -- push the initial state of the private queue. Already in trace, this one gets dropped
               , usedSegments = Set.insert segment $ usedSegments st -- Mark segment as used
               , successorSegs = segSuc $ newSeg -- Record the new successors.
               , stToNetwork = toNetwork $ newSeg }) -- Record the new toNetwork
    _ -> do -- If no public segment fits try private
      when (not toNet) $ progError ("Can't find a successor. \nToNet = False. "
                                    ++ "\n\t Filtered segments:"
                                    ++ show checkedNextSegments
                                    ++ "\n\t unfiltered segments:"
                                    ++ show possibleNextSegments
                                    ++ "\n\t Just successors:"
                                    ++ show succs
                                    ++ ".\n\t PC: "
                                    ++ show currentPc
                                    ++ ".\n\t Network matching the pc: "
                                    ++ show (Map.lookup currentPc avalStates)) -- Check to network!!!
      execSt <- pullStates 1  -- returns singleton list
      pushQueue (head execSt) -- take the element in the list add it to the queue
   where
     -- | Given a segment indicated by the index,
     -- pull enough states to fill the segment and
     -- return the last state
     allocateSegment :: V.Vector (Segment reg MWord) -> Int -> PState reg (ExecutionState reg)
     allocateSegment segments' segmentIndx =
       let segment = segments' V.! segmentIndx
           len = segLen segment in
         do nextStates <- pullStates len
            let newChunk = TraceChunk segmentIndx nextStates
            addChunks [newChunk]
            return $ last nextStates

     -- | Checks if we can use the segment next
     checkSegment :: Bool -> V.Vector (Segment reg MWord) -> Int -> PState reg Bool
     checkSegment verbose segs segInx = do
       let seg = segs V.! segInx
       usedSegs <- usedSegments <$> get
       let available = not $ segInx `Set.member` usedSegs
       trace <- remainingTrace <$> get
       let satLength = length trace >= segLen seg 
       satCons <- satConstraints seg
       when verbose $ T.traceShow ("Index",segInx,"Available:", available,"length:", satLength) (return ()) 
       return $ available && satLength && satCons

     

     -- | Check if segment's constraints are satisfied.
     satConstraints :: Segment reg MWord -> PState reg Bool
     satConstraints seg = do
       sat   <- mapM satConstraint $ constraints seg 
       return $ and sat
     satConstraint :: Constraints -> PState reg Bool
     satConstraint (PcConst pcConst) = do
       pcState <- nextPc <$> get
       return $ pcState == pcConst 
       
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
     -- | If there is an unused segment starting at pc, it returns one of such segment.  
     popSegmentIn :: V.Vector (Segment reg MWord) -> MWord -> PState reg (Maybe Int) 
     popSegmentIn allSegments instrPc = do
       st <- get
       avalStates <- return $ availableSegments st
       case Map.lookup instrPc avalStates of
         Just (execSt' : others) -> do
           segOk <- check (allSegments V.! execSt')
           if segOk then  
             do put $ st {availableSegments = Map.insert instrPc others avalStates}
                return $ Just execSt'
           else return Nothing
         _ -> return Nothing
         where
           -- | This check should make sure the segment we found satisfies the constraints
           --  of the Remaining trace. For now the only check is:
           --  1. Check if there is enough states in the trace to fill the public segment.
           check :: Segment reg MWord -> PState reg Bool
           check seg = do
             trace <- remainingTrace <$> get
             return $ length trace >= segLen seg

-- | Finds private segments to put the states in the queue.
-- It first adds the appropriate stuttering for sparsity and to pad
-- the last segment to have the right ammount of states.
allocateQueue :: Show reg => Int -> PState reg ()
allocateQueue size =
  do queue <- reverse . queueSt <$> get -- FIFO
     spar <- sparsityPS <$> get
     prog <- progPS <$> get
     -- Note: We realy on the fact that the first state never stutters. That state will be dropped.
     let sparseTrace = stutter size spar prog queue 
     currentPrivSegment <- privLoc <$> get
     let tailTrace = tail sparseTrace -- drop the initial state which is already in the trace (in the previous segment)
     let newChunks =  splitPrivBlocks size currentPrivSegment tailTrace
     modify (\st -> st {queueSt = [], privLoc = currentPrivSegment + length newChunks})
     addChunks newChunks

-- | Chunks are added backwards
addChunks :: [TraceChunk reg] -> PState reg ()
addChunks newBlocks = modify (\st -> st {chunksPS = reverse newBlocks ++ chunksPS st})


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
fakeState n = ExecutionState n Map.empty (0, Map.empty) Set.empty [] False False 0

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

_testchunks :: Hopefully [TraceChunk ()]
_testchunks = chooseSegments testPrivSize testSparsity testProg testTrace (V.fromList testSegments')
  where testSparsity = (Map.fromList [(KmemOp, 2)])
        testPrivSize = 4

test = mapM_ (putStrLn . show) <$> _testchunks
