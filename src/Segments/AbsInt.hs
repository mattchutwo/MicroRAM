{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Segments.AbsInt where

import Control.Lens (makeLenses, (^.), (&), (.~), (%~), use, (.=), (%=), ix)
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

import Debug.Trace


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
  _hsCFG :: ProgramCFG
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
initHistoryGraph cfg = HistoryGraph 0 [] cfg

mkHistoryNode :: MonadState (HistoryGraph r) m =>
  [HistoryNode r] -> Maybe [MWord] -> AbsIntState r -> m (HistoryNode r)
mkHistoryNode preds blocks finalState = do
  nodeID <- use hsNextID
  hsNextID %= (+ 1)
  return $ HistoryNode nodeID preds blocks finalState


data Cont r =
  -- | Return to the caller.  There should be only one finished path.
    KReturn (Exec r)
  -- | Join the finished branches together.
  | KJoin (Exec r)
  -- | Terminate the program.  There should either be zero paths (if the
  -- program terminates with `Ianswer`) or one path.
  | KDone

data Exec r = Exec {
  _exLive :: [HistoryNode r],
  _exFinished :: [HistoryNode r],
  -- | The address of the next join point.  Once a live branch reaches this
  -- point, it is moved to finished.
  _exJoin :: MWord,
  -- | The return address of the current function.  This is used to handle
  -- branches with a postdominator of -1, indicating that the branches only
  -- rejoin when the function returns.
  _exReturn :: MWord,
  -- | The continuation: what to do once all branches have finished.
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


stepExec :: Regs r => Exec r -> StateT (HistoryGraph r) Hopefully (Maybe (Exec r))
stepExec ex
  -- If the next path to execute has reached the join point, move it into the
  -- finished list.
  | h:hs <- ex ^. exLive, h ^. histFinalState . sMach . mPc == ex ^. exJoin = do
    return $ Just $ ex & exLive .~ hs & exFinished %~ (h:)

  -- Run one step of the next path.
  | h:hs <- ex ^. exLive = do
    (stop, blocks) <- lift $ runStraightLine' (h ^. histFinalState) (Just $ ex ^. exJoin)
    let stopPc = stop ^. stState . sMach . mPc
    traceM $ "stepExec: ran from " ++ show (h ^. histFinalState . sMach . mPc) ++ " to " ++
      show stopPc ++ ", stop reason = " ++ show (stop ^. stReason)

    let mkHist f = mkHistoryNode [h] (Just blocks) (f $ stop ^. stState)
    -- Generate a history node representing execution of unknown code from `h`,
    -- eventually ending at `pc`.
    let mkHavocHist pc h' = mkHistoryNode [h'] Nothing
          (havocState (h' ^. histFinalState) & sMach . mPc .~ pc)
    let finishBranch h' = do
          -- Record `h'` as an exit, then return `ex` with `h` removed (and not
          -- added to `exFinished`).
          hsExits %= (h':)
          return $ Just $ ex & exLive .~ hs

    -- TODO: correct handling of symbolic branch when ipdom == -1 (return)

    let reason = stop ^. stReason
    let md = stop ^. stMetadata
    case reason of
      -- Direct call, or indirect call with concrete destination
      SrJump dest | mdIsCall md -> do
        let returnAddr = stopPc + 1
        h' <- mkHist $ takeBranch dest
        traceM $ "stepExec: call function " ++ show dest ++ ", return = " ++ show returnAddr
        return $ Just $ Exec [h'] [] returnAddr returnAddr
          (KReturn $ ex & exLive .~ hs) (ex ^. exPendingBranches)
      -- Indirect call with unknown destination
      SrBranch BcAlways VTop | mdIsCall md -> do
        let returnAddr = stopPc + 1
        h' <- mkHist id >>= mkHavocHist returnAddr
        traceM $ "stepExec: call produces havoc, return = " ++ show returnAddr
        return $ Just $ ex & exLive .~ (h':hs)
      _ | mdIsCall md -> error $ "stepExec: impossible: instruction at " ++ show stopPc ++
        " has mdIsCall, but produced unsupported stop reason " ++ show reason

      -- Return with unknown destination.  We force the destination to be
      -- `exReturn`, under the assumption that the code (usually) doesn't
      -- actually overwrite the return address on the stack.  (If this
      -- assumption is violated, the resulting public segments will be
      -- unusable.)
      SrJump dest | mdIsReturn md -> do
        h' <- mkHist $ takeBranch dest
        traceM $ "stepExec: returning to " ++ show dest
        return $ Just $ ex & exLive .~ (h':hs)
      SrBranch BcAlways VTop | mdIsReturn md -> do
        traceM $ "stepExec: return to unknown dest; actually return to " ++ show (ex ^. exReturn)
        h' <- mkHist $ takeBranch (ex ^. exReturn)
        return $ Just $ ex & exLive .~ (h':hs)
      _ | mdIsCall md -> error $ "stepExec: impossible: instruction at " ++ show stopPc ++
        " has mdIsReturn, but produced unsupported stop reason " ++ show reason

      SrJump dest -> do
        h' <- mkHist $ takeBranch dest
        traceM $ "stepExec: jump to " ++ show dest
        return $ Just $ ex & exLive .~ (h':hs)
      SrFallThrough -> do
        h' <- mkHist takeFallThrough
        traceM $ "stepExec: fall through to " ++ show (stopPc + 1)
        return $ Just $ ex & exLive .~ (h':hs)
      SrBranch _ (VExact dest)
        -- This condition limits unrolling of loops with symbolic bounds.  The
        -- first time we hit the loop condition, we explore both sides.  But
        -- the second time, we skip straight to the postdominator (by going out
        -- to the routing network and back) without exploring the branches.
        | not $ IntSet.member (fromIntegral stopPc) (ex ^. exPendingBranches),
          IntSet.size (ex ^. exPendingBranches) < 4 -> do
          h' <- mkHist id
          -- We use these dummy nodes instead of running `mkHist` twice to avoid
          -- duplicating blocks prior to the branch.
          h'1 <- mkHistoryNode [h'] (Just []) (takeBranch dest $ h' ^. histFinalState)
          h'2 <- mkHistoryNode [h'] (Just []) (takeFallThrough $ h' ^. histFinalState)
          optIpdom <- findPostDominator stopPc (ex ^. exReturn)
          case optIpdom of
            Just ipdom -> do
              traceM $ "stepExec: branch, ipdom = " ++ show ipdom
              return $ Just $ Exec [h'1, h'2] [] ipdom (ex ^. exReturn)
                (KJoin $ ex & exLive .~ hs)
                (IntSet.insert (fromIntegral stopPc) (ex ^. exPendingBranches))
            Nothing -> do
              -- TODO: Currently, if ipdom is missing, it means that there is no
              -- path from the block to a return statement.  Currently we discard
              -- such branches immediately, but for some programs it might be
              -- necessary to run them (e.g. due to a bug in error handling
              -- code).
              finishBranch h'
      SrBranch _ _ -> do
        optIpdom <- findPostDominator stopPc (ex ^. exReturn)
        case optIpdom of
          Just ipdom -> do
            --traceShowM ("branch (sym) at", stopPc, "has ipdom", ipdom)
            traceM $ "stepExec: branch produces havoc, ipdom = " ++ show ipdom
            h' <- mkHist id >>= mkHavocHist ipdom
            return $ Just $ ex & exLive .~ (h':hs)
          Nothing -> do
            mkHist id >>= finishBranch
      SrAnswer _ -> do
        mkHist id >>= finishBranch
      SrInvalid -> do
        mkHist id >>= finishBranch
      -- "Requested stop" means this path reached the join point.
      SrRequested -> do
        traceM $ "stepExec: path reached join point " ++ show (ex ^. exJoin) ++
          ", pc = " ++ show stopPc
        h' <- mkHist id
        return $ Just $ ex & exLive .~ hs & exFinished %~ (h':)

  -- All paths have finished, so run the continuation
  | [] <- ex ^. exLive = case ex ^. exCont of
    KReturn ex'
      -- Continue from here in the caller.
      | [h] <- ex ^. exFinished ->
        return $ Just $ ex' & exLive %~ (h:)
      -- If no path reached the return statement, discard the path that
      -- performed this function call.
      | [] <- ex ^. exFinished -> return $ Just ex'
      | otherwise -> error $ "impossible: KReturn received multiple finished paths"
    KJoin ex'
      | [] <- ex ^. exFinished -> return $ Just ex'
      | otherwise -> do
        -- Create a dummy node that joins all the paths, then continue running
        -- that path in the parent context.
        h <- joinPaths (ex ^. exFinished)
        return $ Just $ ex' & exLive %~ (h:)
    KDone
      | [h] <- ex ^. exFinished -> do
        hsExits %= (h:)
        return Nothing
      | [] <- ex ^. exFinished -> do
        return Nothing
      | otherwise -> error $ "impossible: KDone received multiple finished paths"

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
  traceM $ describeExec ex
  ex' <- stepExec ex
  case ex' of
    Just ex'' -> runExec ex''
    Nothing -> return ()

testAbsInt_v :: Regs r =>
  ProgAndMem (AnnotatedProgram Metadata r MWord) -> ProgramCFG -> Hopefully ()
testAbsInt_v prog cfg = evalStateT go (initHistoryGraph cfg)
  where
    s0 = initState prog
    go = do
      lift $ renderControlFlowGraphviz cfg
      initExec s0 >>= runExec
      hg <- get
      lift $ renderHistoryGraphviz hg
      let cg = condenseGraph hg
      let segs = condensedToSegments (pmProg prog) cfg cg
      lift $ renderSegmentsGraphviz segs

renderHistoryGraphviz :: HistoryGraph r -> Hopefully ()
renderHistoryGraphviz hg = evalStateT (render hg) IntSet.empty
  where
    render hg = do
      traceM "digraph {"
      mapM_ go $ hg ^. hsExits
      traceM "}"

    go h = do
      seen <- gets $ IntSet.member (fromIntegral $ h ^. histID)
      when (not seen) $ do
        modify $ IntSet.insert (fromIntegral $ h ^. histID)
        traceM $ show (h ^. histID) ++ " [label = " ++ show (show $ h ^. histBlocks) ++ "];"
        forM_ (h ^. histPreds) $ \h' -> do
          traceM $ show (h' ^. histID) ++ " -> " ++ show (h ^. histID) ++ ";"
          go h'

renderControlFlowGraphviz :: ProgramCFG -> Hopefully ()
renderControlFlowGraphviz p = do
  traceM "digraph {"
  forM_ (IntMap.toList $ p ^. pSuccs) $ \(a, bs) -> do
    forM_ (IntSet.toList bs) $ \b -> do
      traceM $ show a ++ " -> " ++ show b ++ ";"
  forM_ (IntMap.toList $ p ^. pIpdom) $ \(a, b) -> do
      traceM $ show a ++ " -> " ++ show b ++ " [color = blue];"
  traceM "}"

renderSegmentsGraphviz :: [Segment r MWord] -> Hopefully ()
renderSegmentsGraphviz ss = do
  traceM "digraph {  // segments"
  forM_ (zip [0..] ss) $ \(i, s) -> do
    traceM $ "seg" ++ show i ++ " [ label = " ++ show (show $ constraints s) ++ " ];"
    when (fromNetwork s) $ do
      traceM $ "fromnet" ++ show i  ++ " [ label = \"*\" ];"
      traceM $ "fromnet" ++ show i ++ " -> seg" ++ show i ++ ";"
    when (toNetwork s) $ do
      traceM $ "tonet" ++ show i  ++ " [ label = \"*\" ];"
      traceM $ "seg" ++ show i ++ " -> tonet" ++ show i ++ ";"
    forM_ (segSuc s) $ \j -> do
      traceM $ "seg" ++ show i ++ " -> seg" ++ show j ++ ";"
  traceM "}  // segments"


-- | Convert a HistoryGraph into a CondensedGraph, which summarizes the
-- connections between nonempty nodes.  (Blocks with `histNodes` set to
-- `Nothing` are network nodes, those with `Just []` are empty nodes, and all
-- other nodes are nonempty.)
condenseGraph :: HistoryGraph r -> CondensedGraph r
condenseGraph hg = execState (mapM_ (go Nothing) (hg ^. hsExits)) initCG
  where
    initCG = CondensedGraph Map.empty Nothing

    -- | Visit node `n` in the graph traversal.  `succ` is either `Just
    -- nodeID`, indicating that the last interesting successor is `nodeID`, or
    -- `Nothing`, indicating that the last interesting successor is a network
    -- node.
    go succ n = case n ^. histBlocks of
      Nothing -> do
        case succ of
          Nothing -> return ()
          Just succ' -> cgNodes . ix succ' . ciFromNetwork .= True
        mapM_ (go Nothing) (n ^. histPreds)
      Just [] -> do
        mapM_ (go succ) (n ^. histPreds)
      Just (blk:blks) -> do
        seen <- Map.member (n ^. histID) <$> use cgNodes
        when (not seen) $ do
          cgNodes %= Map.insert (n ^. histID)
            (CondensedInfo n (Seq.fromList $ blk : blks) Set.empty False False)
          mapM_ (go (Just $ n ^. histID)) (n ^. histPreds)
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
