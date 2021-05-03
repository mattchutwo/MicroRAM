{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-
Module      : Choosing Segments
Description : 
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Segments.ChooseSegments where

import qualified Data.Graph as G
import Data.Foldable (toList)

import MicroRAM
import MicroRAM.MRAMInterpreter

import qualified Data.Map as Map 
import qualified Data.Set as Set 
import qualified Data.Vector as V (Vector, (!), fromList, ifoldl, imap)

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
    segmentSets = segMap
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
    segPc seg = getPcFromConstraint (segConstraints seg)
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



-- ## Path search 
data PathSearchState reg = PathSearchState {
  remTracePSS :: Trace reg
  , segIndxPSS :: Int
  }

instance Show (PathSearchState reg) where
  show (PathSearchState _ idx) = "PS" ++ show idx
    
instance Eq (PathSearchState reg) where
  (==) pss1 pss2 = (segIndxPSS pss1) == (segIndxPSS pss2) 

instance Ord (PathSearchState reg) where
  compare pss1 pss2 = (segIndxPSS pss1) `compare` (segIndxPSS pss2) 


-- | Finds a path of public segments that conforms to the given trace.
-- We use depth first search, which will find A solution if one exists.
-- TODO: Find longest path  
findPublicPath' :: forall reg. Show reg
               => V.Vector (Segment reg MWord)
               -> Set.Set Int
               -> [Int]
               -> Trace reg -> [Int] 
findPublicPath' segments usedSegs startIndx trace =  
  let allPaths = G.dfs segmGraph startIndx
      result = longestPathForest constrs hasToNetwork $ allPaths
      toNetworks = toList $ V.imap (\i seg -> (i, toNetwork seg)) segments in
    if null $ startIndx then result else
      -- T.trace ("GRAPH: " ++ show segmGraph ++
      --          "\nSTART: " ++ show startIndx ++
      --          "\nEND: " ++ show (toNetworks) ++
      --          "\nALL PATHS: " ++ show allPaths ++
      --          "\n\tRESULT:" ++ show result)
      result 
  where hasToNetwork :: G.Vertex -> Bool
        hasToNetwork segIdx = toNetwork $ segments V.! segIdx

        segmGraph :: G.Graph
        (segmGraph, _) = G.graphFromEdges' $ toList $ V.imap buildNode segments

        buildNode :: Int -> Segment reg wrd -> (Int, Int, [Int])
        buildNode indx seg = (indx, indx, filter notUsed $ segSuc seg)
        notUsed :: Int -> Bool
        notUsed segIndx = not $ segIndx `Set.member` usedSegs
        
        constrs :: [(G.Vertex -> Bool)]
        constrs = map (\est idx -> pcIs (pc est) idx) trace
        pcIs pcTocheck segIndx = pcTocheck == getPcConstraint (segConstraints $ segments V.! segIndx)
          where getPcConstraint (PcConst thePc:_) = thePc  -- Public segments allways have a pc
                getPcConstraint [] = 404 -- This should never happen


longestPathForest :: [(G.Vertex -> Bool)] -> (G.Vertex -> Bool) -> G.Forest G.Vertex -> [G.Vertex]
longestPathForest constraints end paths =
  maxWith length [] $ map (longestPathTree constraints) paths
  where longestPathTree :: [(G.Vertex -> Bool)] -> G.Tree G.Vertex -> [G.Vertex]
        longestPathTree constrs (G.Node v forest) =
          case constrs of
            [] -> []
            constr: constrs' ->
              if not $ constr v then [] else -- Must satisfy the contraint. 
                case longestPathForest constrs' end forest of
                  hd:tl ->  v : hd : tl              -- If the path is not empty    
                  []  -> if end v then [v] else [] -- Otherwise

        maxWith :: (Ord n, Foldable f) => (a -> n) -> a -> f a -> a
        maxWith f def ls = foldl (\path other -> if f path < f other then other else path) def ls

                             
longestPath :: (Ord state, Show (t state), Show state, Foldable t, Functor t)
            => (state -> t state) -- Next
            -> (state -> Bool)  -- End
            -> t state -- START
            -> [state]
longestPath next end start = longestPath' Set.empty start 
  where longestPath' visited start' =
          let paths = fmap (longestPathOne visited) start' in
            maxWith length [] paths
        longestPathOne visited start'
          | start' `Set.member` visited =  [] -- looped
          | otherwise =
            let visited' = Set.insert start' visited 
                path = longestPath' visited' $ next start' in
              if not $ null path then
                start' : path
              else if end start' then [start'] else  []

        maxWith :: (Ord n, Foldable f) => (a -> n) -> a -> f a -> a --
        maxWith f def ls = foldl (\path other -> if f path < f other then other else path) def ls


findPublicPath :: forall reg. Show reg
               => V.Vector (Segment reg MWord)
               -> Set.Set Int
               -> [Int] -> Trace reg -> [Int] 
findPublicPath segments usedSegs startIndx initRemTrace = 
  let result = map segIndxPSS $ longestPath nextStates hasToNetwork initState in
    if null (filter notUsed startIndx) then result else
      result
  where initState :: [PathSearchState reg]
        initState = map (PathSearchState initRemTrace) $ filter notUsed startIndx 

        hasToNetwork :: PathSearchState reg -> Bool
        hasToNetwork pss = toNetwork $ segments V.! (segIndxPSS pss) 

        nextStates :: PathSearchState reg -> [PathSearchState reg]
        nextStates pss =
          let seg = segments V.! (segIndxPSS pss)
              (usedTrace,trimTrace) = splitAt (segLen seg) (remTracePSS pss)
              allSuccIndx =  filter notUsed $ segSuc seg
              pcSuccIndx  =  filter (pcIs $ pc $ last usedTrace) allSuccIndx 
              result = map (PathSearchState trimTrace) $ pcSuccIndx in
            result
        notUsed :: Int -> Bool
        notUsed segIndx = not $ segIndx `Set.member` usedSegs
        pcIs pcTocheck segIndx =
          pcTocheck == getPcConstraint (segConstraints $ segments V.! segIndx)
          where segPc = getPcConstraint (segConstraints $ segments V.! segIndx)
                getPcConstraint (PcConst thePc:_) = thePc
                --getPcConstraint (_:ls) = getPcConstraint ls
                getPcConstraint [] = error "No PC constraint found on public segment."
                
-- | chooses the next segment
chooseSegment :: Show reg => V.Vector (Segment reg MWord) -> Int -> PState reg ()
chooseSegment segments privSize = do
  thePc <- nextPc <$> get
  initRemTrace <- remainingTrace <$> get
  avalSegs <-  availableSegments <$> get
  usedSegs <-  usedSegments <$> get
  let startInds = Map.findWithDefault [] thePc avalSegs
  let possiblePath = findPublicPath segments usedSegs startInds initRemTrace
  case possiblePath of
    x:path -> do
      -- allocates the current queue in private pc segments (if there is more than just the last state).  
      queue <- queueSt <$> get
      when (length queue >1) $ allocateQueue privSize
      -- allocates states to use the public pc segment. Returns the last state (now the start of the queue)
      queueInitSt <- mapM (allocateSegment segments) (x:path)
      modify (\st -> st {queueSt = [last queueInitSt] -- push the initial state of the private queue. Already in trace, this one gets dropped
               , usedSegments = (Set.fromList (x:path)) `Set.union` usedSegs -- Mark path as used (TODO: Move to allocatePublicPath)
               , nextPc = pc $ last queueInitSt }) 
    [] -> do -- If no public segment fits try private
      execSt <- pullStates 1  -- returns singleton list
      pushQueue (head execSt) -- take the element in the list add it to the queue
   where
     -- | Given a segment indicated by the index,
     -- pull enough states to fill the segment and
     -- return the last state
     allocateSegment :: V.Vector (Segment reg MWord) -> Int -> PState reg (ExecutionState reg)
     allocateSegment segments' segmentIndx =do
       nextStates <- pullStates len
       let newChunk = TraceChunk segmentIndx nextStates
       addChunks [newChunk]
       modify (\st -> st{usedSegments = segmentIndx `Set.insert` usedSegments st})
       return $ last nextStates
         where segment = segments' V.! segmentIndx
               len = segLen segment

     -- | Checks if we can use the segment next
     checkSegment :: V.Vector (Segment reg MWord) -> Int -> PState reg Bool
     checkSegment segs segInx = do
       let seg = segs V.! segInx
       usedSegs <- usedSegments <$> get
       let available = not $ segInx `Set.member` usedSegs
       trace <- remainingTrace <$> get
       let satLength = length trace >= segLen seg 
       satCons <- satConstraints seg
       return $ available && satLength && satCons

     

     -- | Check if segment's constraints are satisfied.
     satConstraints :: Segment reg MWord -> PState reg Bool
     satConstraints seg = do
       sat   <- mapM satConstraint $ segConstraints seg 
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
