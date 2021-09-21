{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Segments.AbsInt where

import Control.Lens (makeLenses, (^.), (^?), (&), (.~), (%~), use, (.=), (%=), ix, at)
import Control.Monad.State
import Data.Bits
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.IRs
import Compiler.Metadata
import Compiler.Registers
import MicroRAM
import MicroRAM.MRAMInterpreter
import MicroRAM.MRAMInterpreter.AbsInt
import MicroRAM.MRAMInterpreter.Generic (InterpState'(..), MachineState'(..), AbsDomain(..))
import Segments.ControlFlow
import Segments.Segmenting
import Util.Util (showHex)


data HistoryNode r = HistoryNode {
  _histID :: Word,
  _histPreds :: [HistoryNode r],
  _histBlocks :: Maybe [MWord],
  _histFinalState :: AbsIntState r
}
makeLenses ''HistoryNode

data HistoryGraph r = HistoryGraph {
  _hsNextID :: Word,
  _hsExits :: [HistoryNode r],
  _hsCFG :: ProgramCFG,
  -- | Additional regions to execute.  The region `(pc1, pc2)` means execution
  -- should start at `pc1` and continue until it reaches `pc2`.
  _hsPendingRegions :: [(AbsIntState r, MWord)],
  -- | All regions that have been seen so far, together with how many times the
  -- region was seen.  Used to avoid queuing more than some maximum number of
  -- instances of the same static region.
  _hsSeenRegions :: Map (MWord, MWord) Word,
  -- | Maximum number of instances to produce for each region.  If a region is
  -- missing from this map, its limit will be computed on first access.
  _hsRegionInstanceLimit :: Map (MWord, MWord) Word
}
makeLenses ''HistoryGraph


data CondensedInfo r = CondensedInfo {
  _ciNode :: HistoryNode r,
  _ciBlocks :: Seq MWord,
  _ciSuccs :: Set Word,
  _ciFromNetwork :: Bool,
  _ciToNetwork :: Bool
}

data CondensedGraph r = CondensedGraph {
  _cgNodes :: Map Word (CondensedInfo r),
  _cgEntry :: Maybe Word
}

makeLenses ''CondensedInfo
makeLenses ''CondensedGraph


initHistoryGraph :: ProgramCFG -> HistoryGraph r
initHistoryGraph cfg = HistoryGraph 0 [] cfg mempty mempty mempty

mkHistoryNode :: MonadState (HistoryGraph r) m =>
  [HistoryNode r] -> Maybe [MWord] -> AbsIntState r -> m (HistoryNode r)
mkHistoryNode preds blocks finalState = do
  nodeID <- use hsNextID
  hsNextID %= (+ 1)
  return $ HistoryNode nodeID preds blocks finalState

-- | Heuristic for determining the maximum number of instances to generate for
-- a given region.
regionInstanceLimit :: MonadState (HistoryGraph r) m =>
  (MWord, MWord) -> m Word
regionInstanceLimit rg = do
  optLimit <- use $ hsRegionInstanceLimit . at rg
  case optLimit of
    Just x -> return x
    Nothing -> do
      cfg <- use hsCFG

      -- The current heuristic: for a region containing `N` static
      -- instructions, generate up to `1000 / N` copies.
      -- TODO: base the 1000 on the trace length, or otherwise make it tunable
      -- TODO: count call instructions as 10x-100x the cost of normal instrs
      let regionSize = calcSize cfg rg
      let limit = if regionSize == 0 then 1 else max 1 $ 1000 `div` regionSize

      hsRegionInstanceLimit . at rg .= Just limit
      return limit
  where
    calcSize :: ProgramCFG -> (MWord, MWord) -> Word
    calcSize cfg (startPc, endPc) =
      sum $ map (blockSize cfg) $ Set.toList $ regionBlocks cfg Set.empty endPc startPc

    regionBlocks :: ProgramCFG -> Set MWord -> MWord -> MWord -> Set MWord
    regionBlocks cfg blks endPc pc
      -- Don't include the end PC, since it's not executed as part of the
      -- region.
      | pc == endPc = blks
      | Set.member pc blks = blks
      | otherwise =
        IntSet.foldr' (\pc' blks' -> regionBlocks cfg blks' endPc (fromIntegral pc'))
          (Set.insert pc blks) succs
      where
        succs = maybe mempty id $ cfg ^? pSuccs . ix (fromIntegral pc)

    blockSize :: ProgramCFG -> MWord -> Word
    blockSize cfg pc = case IntSet.lookupGT (fromIntegral pc) (cfg ^. pBlockStarts) of
      Just pc' -> fromIntegral pc' - fromIntegral pc
      -- Note that `blockStarts` contains the address of the end of the
      -- program, so even the last real block of the program shouldn't enter
      -- the `Nothing` case.
      Nothing -> error $ "blockSize: pc " ++ show pc ++ " is not in any block?"

queueRegion :: MonadState (HistoryGraph r) m =>
  AbsIntState r -> MWord -> m ()
queueRegion s endPc = do
  let rg = (s ^. sMach . mPc, endPc)
  seen <- use hsSeenRegions
  limit <- regionInstanceLimit rg
  when (maybe 0 id (Map.lookup rg seen) < limit) $ do
    hsSeenRegions %= Map.insertWith (+) rg 1
    hsPendingRegions %= ((s, endPc) :)


data Cont r =
  -- | Return to the caller.  There should be only one finished path.
    KReturn (Exec r)
  -- | Join the finished branches together.
  | KJoin (Exec r)
  -- | Terminate the program.  There should either be zero paths (if the
  -- program terminates with `Ianswer`) or one path.
  | KDone

-- | The state of an abstract interpreter execution.  This tracks all the paths
-- that are currently being explored.
--
-- `Exec`s are organized into a stack of scopes.  Each scope is created with
-- some set of paths, which are initially in `exLive`; as paths are processed,
-- they eventually move from `exLive` to `exFinished`; and when all paths are
-- in `exFinished`, the current scope is complete and control returns to the
-- parent scope via the continuation `exCont`.  A new scope is created when
-- processing of a path reaches a function call or a symbolic branch.
data Exec r = Exec {
  -- | Paths from the current scope that are currently executing.
  --
  -- Each path is represented by a `HistoryNode`, which tracks not only the
  -- current program state but also the path(s) through the program that led up
  -- to that state.
  _exLive :: [HistoryNode r],
  -- | Paths from the current scope that have finished executing.
  _exFinished :: [HistoryNode r],
  -- | The address of the next join point.  Once a live branch reaches this
  -- point, it is moved to finished.
  _exJoin :: MWord,
  -- | The return address of the current function.  This is used to handle
  -- branches with a postdominator of -1, indicating that the branches only
  -- rejoin when the function returns.
  _exReturn :: MWord,
  -- | The continuation: what to do once all branches in the current scope have
  -- finished.
  _exCont :: Cont r,
  -- | A set of branch addresses for all pending symbolic branches.
  _exPendingBranches :: IntSet
}
makeLenses ''Exec


findPostDominator :: MonadState (HistoryGraph r) m => MWord -> MWord -> m (Maybe MWord)
findPostDominator pc retAddr = do
  cfg <- use hsCFG
  let branchBlock = case IntSet.lookupLE (fromIntegral pc) (cfg ^. pBlockStarts) of
        Just x -> x
        Nothing -> error $ "impossible: PC " ++ show pc ++ " is not in any block?"
  case IntMap.lookup branchBlock (cfg ^. pIpdom) of
    Just (-1) -> return $ Just retAddr
    Just x -> return $ Just $ fromIntegral x
    Nothing -> return Nothing

joinPaths :: forall r m. (MonadState (HistoryGraph r) m, Regs r) =>
  [HistoryNode r] -> m (HistoryNode r)
joinPaths [] = error $ "impossible: tried to join an empty list of paths"
joinPaths [h] = return h
joinPaths hs = do
  let ss = map (\h -> h ^. histFinalState) hs
  let s' = foldl1 merge ss
  mkHistoryNode hs (Just []) s'
  where
    merge :: AbsIntState r -> AbsIntState r -> AbsIntState r
    merge
      (InterpState ext1 mach1)
      (InterpState ext2 mach2) =
      InterpState
        (mergeExt ext1 ext2)
        (mergeMach mach1 mach2)

    mergeExt
      (ExtraState metadata1)
      (ExtraState metadata2) =
      ExtraState
        (assertEq "metadata" metadata1 metadata2)
    mergeMach
      (MachineState cycle1 pc1 regs1 prog1 mem1 bug1 answer1)
      (MachineState cycle2 pc2 regs2 prog2 mem2 bug2 answer2) =
      MachineState
        (absMerge cycle1 cycle2)
        (assertEq "program counter" pc1 pc2)
        (mergeRegs regs1 regs2)
        (assertEq "program" prog1 prog2)
        (mergeMem mem1 mem2)
        (bug1 && bug2)
        (assertEq "answer" answer1 answer2)

    mergeRegs
      (RegBank m1 d1)
      (RegBank m2 d2) =
      RegBank
        (Map.unionWith absMerge m1 m2)
        (absMerge d1 d2)

    mergeMem
      (AbsMemory mem1 default1 poisonedZero1)
      (AbsMemory mem2 default2 poisonedZero2) =
      AbsMemory
        (Map.fromDistinctAscList $ go (Map.toList mem1) (Map.toList mem2))
        (absMerge default1 default2)
        (poisonedZero1 || poisonedZero2)
      where
        go ::
          [(MWord, (MemWidth, AbsValue))] ->
          [(MWord, (MemWidth, AbsValue))] ->
          [(MWord, (MemWidth, AbsValue))]
        go [] [] = []
        go ((a1, (w1, v1)) : rest1) [] =
          (a1, (w1, absMerge v1 default2)) : go rest1 []
        go [] ((a2, (w2, v2)) : rest2) =
          (a2, (w2, absMerge default1 v2)) : go [] rest2
        go all1@((a1, (w1, v1)) : rest1) all2@((a2, (w2, v2)) : rest2)
          -- Exact match
          | a1 == a2 && w1 == w2 = (a1, (w1, absMerge v1 v2)) : go rest1 rest2
          -- No overlap
          | a2 >= a1 + fromIntegral (widthInt w1) =
            (a1, (w1, absMerge v1 default2)) : go rest1 all2
          | a1 >= a2 + fromIntegral (widthInt w2) =
            (a2, (w2, absMerge default1 v2)) : go all1 rest2
          -- Partial overlap.  If one comes strictly before the other, split
          -- the earlier one in two in order to split off the non-overlapping
          -- prefix.  If the addresses are the same, then split the longer one
          -- in order to split off the non-overlapping suffix.
          | a1 < a2 || (a1 == a2 && w1 > w2) =
            go (splitMem a1 w1 v1 ++ rest1) all2
          | a2 < a1 || (a2 == a1 && w2 > w1) =
            go all1 (splitMem a2 w2 v2 ++ rest2)
          -- The `otherwise` case should be unreachable, as we already covered
          -- each combination of `a1 <=> a2` and `w1 <=> w2`.  But GHC doesn't
          -- recognize this, and reports a nonexhaustive match without it.
          | otherwise = error $ "mergeMem: impossible case: " ++
            show ((a1, w1, v1), (a2, w2, v2))

        splitMem a w v = [(a, (w', vLow)), (a', (w', vHigh))]
          where
            w' = case w of
              W1 -> error $ "mergeMem: impossible: tried to split W1 at " ++ showHex a
              W2 -> W1
              W4 -> W2
              W8 -> W4
            a' = a + fromIntegral (widthInt w')

            bits = 8 * widthInt w'
            vLow = v `absAnd` absExact ((1 `shiftL` bits) - 1)
            vHigh = v `absShr` absExact (fromIntegral bits)

    reallyAssertEq = False
    assertEq desc x1 x2
      | not reallyAssertEq = x1
      | x1 == x2 = x1
      | otherwise = error $ "joinPaths: expected " ++ desc ++ " values to be equal"


-- | Run the abstract interpreter, starting from the execution state `ex`.
-- Returns a new state `Just ex'`, or `Nothing` to indicate that the execution
-- has terminated.  Also adds new nodes to the `HistoryGraph` as a side effect.
--
-- A single call to `stepExec` may run multiple steps of the underlying
-- abstract interpreter, for efficiency.  There is no guarantee as to the
-- number of steps performed; it may be zero, one, or many steps.
stepExec :: Regs r => Exec r -> StateT (HistoryGraph r) Hopefully (Maybe (Exec r))
stepExec ex
  -- There are three top-level cases here:
  -- 1. The first live path is ready to finish, having reached `exJoin`
  -- 2. The first live path is not ready and needs to be run some more
  -- 3. There are no more live paths, and the current scope can finish

  -- (1) If the next path to execute has reached the join point, move it into
  -- the finished list.
  | h:hs <- ex ^. exLive, h ^. histFinalState . sMach . mPc == ex ^. exJoin = do
    return $ Just $ ex & exLive .~ hs & exFinished %~ (h:)

  -- (2) Run the next path until reaching a stopping point.  "Stopping points"
  -- are `exJoin` and any function calls or symbolic branches.  Upon stopping,
  -- this case takes an appropriate action depending on the stop reason.
  | h:hs <- ex ^. exLive = do
    (stop, blocks) <- lift $ runStraightLine' (h ^. histFinalState) (Just $ ex ^. exJoin)
    let stopPc = stop ^. stState . sMach . mPc

    -- Generate a history node representing straight-line execution from `h` to
    -- the stopping point `stop`, using `f` to rewrite the final state.
    let mkHist f = mkHistoryNode [h] (Just blocks) (f $ stop ^. stState)
    -- Generate a history node representing execution of unknown code from
    -- `h'`, eventually ending at `pc`.
    let mkHavocHist pc h' = mkHistoryNode [h'] Nothing
          (havocState (h' ^. histFinalState) & sMach . mPc .~ pc)
    -- Record `h'` as an exit, then return `ex` with `h` removed.  This is used
    -- when a path terminates the program.
    let terminatePath h' = do
          hsExits %= (h':)
          return $ Just $ ex & exLive .~ hs

    let reason = stop ^. stReason
    let md = stop ^. stMetadata
    case reason of
      -- Function calls

      -- Direct call, or indirect call with concrete destination.  Enter the
      -- function in a new scope with a `KReturn` continuation.
      SrJump dest | mdIsCall md -> do
        let returnAddr = stopPc + 1
        h' <- mkHist $ takeBranch dest
        -- Note we remove the current path from `exLive` here, effectively
        -- transferring ownership into the new scope.  Once the new scope
        -- finishes, the updated path will be transferred back and added back
        -- to `exLive`.
        return $ Just $ Exec [h'] [] returnAddr returnAddr
          (KReturn $ ex & exLive .~ hs) (ex ^. exPendingBranches)
      -- Indirect call with unknown destination.  Skip the call (since we can't
      -- execute an unknown PC) and havoc the state instead.
      SrBranch BcAlways VTop | mdIsCall md -> do
        let returnAddr = stopPc + 1
        h' <- mkHist id >>= mkHavocHist returnAddr
        return $ Just $ ex & exLive .~ (h':hs)
      _ | mdIsCall md -> error $ "stepExec: impossible: instruction at " ++ show stopPc ++
        " has mdIsCall, but produced unsupported stop reason " ++ show reason

      -- Returns

      -- For all return instructions, regardless of stop reason, we just
      -- blindly jump to `exReturn`, under the assumption that the code doesn't
      -- overwrite the return address on the stack.  If this assumption is
      -- violated, the resulting public segments will be unusable.
      _ | mdIsReturn md -> do
        h' <- mkHist $ takeBranch (ex ^. exReturn)
        return $ Just $ ex & exLive .~ (h':hs)

      -- All other instructions

      -- Take any concrete branches or fallthroughs, and update the current
      -- path.  These cases should never happen, since `runStraightLine'` is
      -- supposed to proceed through such jumps, but we include them here just
      -- in case.
      SrJump dest -> do
        h' <- mkHist $ takeBranch dest
        return $ Just $ ex & exLive .~ (h':hs)
      SrFallThrough -> do
        h' <- mkHist takeFallThrough
        return $ Just $ ex & exLive .~ (h':hs)

      -- A branch with a concrete destination but a symbolic condition.  We
      -- fork into two paths, one that takes the branch and another that falls
      -- through, and execute them both in a new scope.
      SrBranch _ (VExact dest)
        -- The first guard here limits unrolling of loops with symbolic bounds
        -- within a single execution.  The first time we hit the loop
        -- condition, we explore both sides.  But the second time, we skip
        -- straight to the postdominator (applying havoc to the program state)
        -- without exploring the branches.
        | not $ IntSet.member (fromIntegral stopPc) (ex ^. exPendingBranches),
          -- The second guard limits the number of symbolic branches that will
          -- be explored in parallel.  If the current path is nested within at
          -- least N symbolic branches, then it isn't allowed to branch any
          -- more.  This limits the number of parallel branches in the
          -- execution graph to `2^N`.
          IntSet.size (ex ^. exPendingBranches) < 4 -> do

          -- `h'` is the common history leading up to just before the branch.
          -- `h'1` and `h'2` are the two forked paths, with one taking the
          -- branch and the other falling through.
          h' <- mkHist id
          h'1 <- mkHistoryNode [h'] (Just []) (takeBranch dest $ h' ^. histFinalState)
          h'2 <- mkHistoryNode [h'] (Just []) (takeFallThrough $ h' ^. histFinalState)
          optIpdom <- findPostDominator stopPc (ex ^. exReturn)
          case optIpdom of
            Just ipdom -> do
              -- This new scope will execute both branches separately, then
              -- join them and continue with the current path.  Like in the
              -- function call case, ownership is transferred into the new
              -- scope and the resulting path is returned when it finishes.
              return $ Just $ Exec [h'1, h'2] [] ipdom (ex ^. exReturn)
                (KJoin $ ex & exLive .~ hs)
                (IntSet.insert (fromIntegral stopPc) (ex ^. exPendingBranches))
            Nothing -> do
              -- If ipdom is missing, it means that there is no path from the
              -- block to a return statement.  Currently we discard such
              -- paths immediately, but for some programs it might be
              -- beneficial to run them (e.g. for a bug in error handling code,
              -- we might want public segments covering that code).
              terminatePath h'

      -- A branch where either the destination is symbolic, or the conditions
      -- on the forking case didn't pass.
      --
      -- Currently we don't generate conditional branches to non-constant
      -- addresses, so for now we only get here when the forking conditions
      -- fail.
      SrBranch _ dest -> do
        optIpdom <- findPostDominator stopPc (ex ^. exReturn)
        case optIpdom of
          Just ipdom -> do
            -- Skip to the postdominator without running either branch.
            h' <- mkHist id >>= mkHavocHist ipdom
            case dest of
              VExact dest' -> do
                -- When the branches we're skipping over are known, queue them
                -- to be explored independently later on.
                queueRegion (havocState $ takeBranch dest' (stop ^. stState)) ipdom
                queueRegion (havocState $ takeFallThrough (stop ^. stState)) ipdom
              _ -> return ()
            return $ Just $ ex & exLive .~ (h':hs)
          Nothing -> do
            -- As in the forking case, missing ipdom means we discard the path.
            mkHist id >>= terminatePath

      -- "Answer" and "invalid" are both ways of terminating execution.
      SrAnswer _ -> do
        mkHist id >>= terminatePath
      SrInvalid -> do
        mkHist id >>= terminatePath

      -- "Requested stop" means this path reached the join point.
      SrRequested -> do
        -- Just update the current path.  The next call to `stepExec` should
        -- see that the path is at its join point and move it to `exFinished`.
        -- We don't inline that logic here, for consistency.
        h' <- mkHist id
        return $ Just $ ex & exLive .~ (h':hs)

  -- (3) All paths in the current scope have finished.  Apply the continuation.
  | [] <- ex ^. exLive = case ex ^. exCont of
    -- In general, the number of finished paths can't exceed the number of
    -- paths in `exLive` when the current scope was created.  But it's possible
    -- for the number to be smaller (even zero), as some paths might terminate
    -- early.

    KReturn ex'
      -- The function returned normally.  Transfer the resulting path back to
      -- the parent scope, where it will continue running.
      | [h] <- ex ^. exFinished ->
        return $ Just $ ex' & exLive %~ (h:)
      -- No path reached the return statement, so discard the path that entered
      -- the call.
      | [] <- ex ^. exFinished -> return $ Just ex'
      -- Scopes ending with `KReturn` are created with only one path, so it
      -- should be impossible to have multiple paths here.
      | otherwise -> error $ "impossible: KReturn received multiple finished paths"

    KJoin ex'
      -- After forking, all of the new paths terminated before reaching the
      -- join point.
      | [] <- ex ^. exFinished -> return $ Just ex'
      -- At least one path reached the join point.
      | otherwise -> do
        -- Create a dummy node that joins all the paths, then continue running
        -- that path in the parent context.
        h <- joinPaths (ex ^. exFinished)
        return $ Just $ ex' & exLive %~ (h:)

    KDone
      | [h] <- ex ^. exFinished -> do
        -- Record the final path as terminating the execution, like in
        -- `terminatePath` above.
        hsExits %= (h:)
        return Nothing
      | [] <- ex ^. exFinished -> do
        return Nothing
      -- Scopes ending with `KDone` are created with only one path, so it
      -- should be impossible to have multiple paths here.
      | otherwise -> error $ "impossible: KDone received multiple finished paths"

-- | Construct the initial `Exec` state for starting from program state `s`.
-- In practice, `s` is usually the initial program state.
initExec :: AbsIntState r -> StateT (HistoryGraph r) Hopefully (Exec r)
initExec s = do
  h <- mkHistoryNode [] (Just []) s
  optIpdom <- findPostDominator (s ^. sMach . mPc) (fromIntegral $ -1)
  let ipdom = case optIpdom of
        Just x -> x
        -- For functions that don't return, we use a dummy join/return address.
        Nothing -> -1
  return $ Exec [h] [] ipdom ipdom KDone IntSet.empty

describeExec :: Exec r -> String
describeExec ex = "Exec: " ++
  "live " ++ show (map (\h -> h ^. histFinalState . sMach . mPc) (ex ^. exLive)) ++
  ", finished " ++ show (length $ ex ^. exFinished) ++
  ", join " ++ show (ex ^. exJoin) ++
  ", return " ++ show (ex ^. exReturn) ++
  ", cont " ++ contDesc ++ " (depth " ++ show (contDepth $ ex ^. exCont) ++ ")"
  where
    contDesc = case ex ^. exCont of
      KJoin _ -> "KJoin"
      KReturn _ -> "KReturn"
      KDone -> "KDone"

    contDepth (KJoin ex) = 1 + contDepth (ex ^. exCont)
    contDepth (KReturn ex) = 1 + contDepth (ex ^. exCont)
    contDepth KDone = 0

runExec :: Regs r => Exec r -> StateT (HistoryGraph r) Hopefully ()
runExec ex = do
  ex' <- stepExec ex
  case ex' of
    Just ex'' -> runExec ex''
    Nothing -> runNextExec

runNextExec :: Regs r => StateT (HistoryGraph r) Hopefully ()
runNextExec = do
  pending <- use hsPendingRegions
  case pending of
    [] -> return ()
    ((s, endPc) : pending') -> do
      hsPendingRegions .= pending'
      h <- mkHistoryNode [] Nothing s
      let exec = Exec [h] [] endPc endPc KDone IntSet.empty
      runExec exec


-- | Convert a HistoryGraph into a CondensedGraph, which summarizes the
-- connections between nonempty nodes.  (Blocks with `histNodes` set to
-- `Nothing` are network nodes, those with `Just []` are empty nodes, and all
-- other nodes are nonempty.)
condenseGraph :: HistoryGraph r -> CondensedGraph r
condenseGraph hg = execState (mapM_ (\x -> go Nothing Nothing x) (hg ^. hsExits)) initCG
  where
    initCG = CondensedGraph Map.empty Nothing

    -- | Visit node `n` in the graph traversal.  `succ` is either `Just
    -- nodeID`, indicating that the last interesting successor is `nodeID`, or
    -- `Nothing`, indicating that the last interesting successor is a network
    -- node.
    go succ realSucc n = case n ^. histBlocks of
      Nothing -> do
        case succ of
          Nothing -> return ()
          Just succ' -> cgNodes . ix succ' . ciFromNetwork .= True
        mapM_ (go Nothing realSucc) (n ^. histPreds)
      Just [] -> do
        mapM_ (go succ realSucc) (n ^. histPreds)
      Just (blk:blks) -> do
        seen <- Map.member (n ^. histID) <$> use cgNodes
        when (not seen) $ do
          let init = case realSucc of
                Just x -> Set.singleton x
                Nothing -> Set.empty
          cgNodes %= Map.insert (n ^. histID)
            (CondensedInfo n (Seq.fromList $ blk : blks) init False False)
          mapM_ (go (Just $ n ^. histID) (Just $ n ^. histID)) (n ^. histPreds)
        case succ of
          Nothing -> cgNodes . ix (n ^. histID) . ciToNetwork .= True
          Just succ' -> cgNodes . ix (n ^. histID) . ciSuccs %= Set.insert succ'
        when (blk == 0) $ do
          cgEntry .= Just (n ^. histID)


condensedToSegments :: forall r.
  AnnotatedProgram Metadata r MWord ->
  ProgramCFG ->
  CondensedGraph r ->
  [Segment r MWord]
condensedToSegments prog cfg cg = [mkSeg w info b | (w, info, b) <- segDescs]
  where
    getInfo :: Word -> CondensedInfo r
    getInfo w = case Map.lookup w (cg ^. cgNodes) of
      Just x -> x
      Nothing -> error $ "condensedToSegments: CondensedGraph referenced unknown ID " ++ show w

    segDescs :: [(Word, CondensedInfo r, Int)]
    segDescs =
      -- Put the entry node first, if one was found.
      [(w, info, b) | w <- toList (cg ^. cgEntry), info <- [getInfo w],
        b <- [0 .. Seq.length (info ^. ciBlocks) - 1]] ++
      [(w, info, b) | (w, info) <- Map.toList (cg ^. cgNodes), Just w /= cg ^. cgEntry,
        b <- [0 .. Seq.length (info ^. ciBlocks) - 1]]

    -- | Map from history node IDs to segment indices.
    idMap :: Map (Word, Int) Int
    idMap = Map.fromList $ zip (map (\(w, _, b) -> (w, b)) segDescs) [0..]

    convertID :: Word -> Int -> Int
    convertID w b = case Map.lookup (w, b) idMap of
      Just i -> i
      Nothing -> error $ "CondensedGraph node referenced unknown ID " ++ show w

    mkSeg w info b = Segment {
        segIntrs = instrs,
        constraints = [PcConst pc],
        segLen = length instrs,
        segSuc = if isLast then
            map (\w -> convertID w 0) (Set.toList $ info ^. ciSuccs)
          else
            [convertID w (b + 1)],
        fromNetwork = isFirst && info ^. ciFromNetwork,
        toNetwork = isLast && info ^. ciToNetwork
      }
      where
        isFirst = b == 0
        isLast = b == Seq.length (info ^. ciBlocks) - 1

        progInstrs = Seq.fromList $ map fst prog

        pc = case Seq.lookup b (info ^. ciBlocks) of
          Just x -> x
          Nothing -> error $ "condensedToSegments: block " ++ show b ++
            " out of range for node " ++ show w
        pc' = case IntSet.lookupGT (fromIntegral pc) (cfg ^. pBlockStarts) of
            Just pc' -> fromIntegral pc'
            Nothing -> fromIntegral $ Seq.length progInstrs
        instrs = toList $ Seq.take (fromIntegral $ pc' - pc) $
          Seq.drop (fromIntegral pc) $ progInstrs

segmentProgramWithAbsInt :: Regs r =>
  ProgAndMem (AnnotatedProgram Metadata r MWord) ->
  Hopefully [Segment r MWord]
segmentProgramWithAbsInt pm = do
  let cfg = buildProgramCFG (pmProg pm)
  hg <- execStateT (initExec (initState pm) >>= runExec) (initHistoryGraph cfg)
  let cg = condenseGraph hg
  return $ condensedToSegments (pmProg pm) cfg cg
