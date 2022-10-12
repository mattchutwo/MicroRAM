{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : RemovePhi
Description : Remove CFG edges leading into phi instructions
Maintainer  : santiago@galois.com
Stability   : Prototype

This removes all `RPhi` instructions from an RTL program by splitting the
incoming edges and adding `Imov`s in the newly introduced blocks.
-}

module Compiler.RemovePhi
    ( edgeSplit
    , removePhi
    ) where

import Control.Monad.State
import qualified Data.Graph.Directed as Digraph
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)

import Compiler.Common (Name(..))
import Compiler.Errors
import Compiler.IRs
import Compiler.Metadata
import Compiler.RegisterAlloc.Liveness
import MicroRAM

data NameState = NameState
  { _nextName :: Word,
    nameMap :: Map.Map (Name, Name) Name}

-- Split edges to ensure the "unique successor or predecessor property" (Modern Compiler Implementation in Java, 19.6).
edgeSplit :: (Rprog Metadata wrdT, Word) -> Hopefully (Rprog Metadata wrdT, Word)
edgeSplit (IRprog te ge funcs ext, nextReg) = do
  let (funcs', NameState nextReg' _) = runState (mapM edgeSplitFunc funcs) initNameState
  return $ (IRprog te ge funcs' ext, nextReg')
  where initNameState = NameState nextReg Map.empty

edgeSplitFunc :: forall wrdT . RFunction Metadata wrdT -> State NameState (RFunction Metadata wrdT)
edgeSplitFunc (Function name retTy argTys argNms blocks extern) = do
  -- Function starts with empy name map
  modify (\st -> st {nameMap = Map.empty})
  -- create spliting blocks
  edgeBlocks <- mapM (buildEdgeBlock name) $ Map.toList edgeIndex
  -- Modify existing blocks
  blocks' <- mapM fixInstrs blocks
  return $ Function name retTy argTys argNms (blocks' ++ edgeBlocks) extern

  where
    (cfg, predecessorMap, successorMap) = buildCFG blocks

    -- Build new blocks to place along the previous CFG edges.

    buildEdgeBlock :: Name -> ((Name, Name), Int) -> State NameState (BB Name (RTLInstr Metadata wrdT))
    buildEdgeBlock funcName ((pred, succ), edgeIdx) = do
      name <- makeEdgeBlockName pred succ edgeIdx
      let body = []
      let md = trivialMetadata funcName name
      let term = [MRI (Ijmp $ Label succ) md] -- This will eventually be deleted
      return $ BB name body term [succ]

    -- | Map from (pred, succ) to a unique index.  This is mainly used to
    -- ensure that every new block gets a unique name, even if there are
    -- collisions such as `Name "0"` vs. `NewName 0`.
    edgeIndex :: Map (Name, Name) Int
    edgeIndex = 
      let splitEdges = Set.toList $ Set.unions $ 
            -- Iterate over nodes.
            let ns = Digraph.nodes cfg in

            map (\nid ->
                -- Get successors.
                let succs = getSuccessors nid in

                -- Skip if less than 2 successors.
                if List.length succs < 2 then
                  mempty
                else
                  Set.unions $ map (\succId -> 
                      -- Get predecessors of each successor.
                      let succPreds = getPredecessors succId in

                      -- Include if more than one predecessor.
                      if List.length succPreds > 1 then
                        Set.singleton (nid, succId)
                      else
                        mempty
                    ) succs
              ) ns
      in

      Map.fromList $ zip splitEdges [0..]

    getSuccessors v = maybe [] id $ Map.lookup v successorMap
    getPredecessors v = maybe [] id $ Map.lookup v predecessorMap
    
    fixInstrs :: BB Name (RTLInstr Metadata wrdT) -> State NameState (BB Name (RTLInstr Metadata wrdT))
    fixInstrs (BB name body term dag) = do
      dag' <- fixDag name dag
      body' <- mapM (fixInstr name) body
      term' <- mapM (fixInstr name) term
      return $ BB name body' term' dag'

    fixInstr :: Name -> RTLInstr Metadata wrdT -> State NameState (RTLInstr Metadata wrdT)
    fixInstr pred (MRI (Ijmp (Label succ)) mdata) = MRI . Ijmp <$> jumpDest pred succ <*> return mdata
    fixInstr pred (MRI (Icjmp r (Label succ)) mdata) = MRI . (Icjmp r) <$> (jumpDest pred succ) <*> return mdata
    fixInstr pred (MRI (Icnjmp r (Label succ)) mdata) = MRI . (Icnjmp r) <$> (jumpDest pred succ) <*> return mdata
    fixInstr succ (IRI (RPhi r vs) mdata) = do
      vs' <- mapM fixSuccs vs
      return $ IRI (RPhi r $ vs') mdata
      where fixSuccs (o, pred) = do {pred' <- fixPred pred succ; return (o, pred')} 
    fixInstr _ instr = return instr

    jumpDest :: Name -> Name -> State NameState (MAOperand VReg wrdT)
    jumpDest pred succ = Label <$> fixSucc pred succ

    fixDag :: Name -> DAGinfo Name -> State NameState (DAGinfo Name) 
    fixDag pred succs = mapM (fixSucc pred) succs

    fixSucc pred succ = case Map.lookup (pred, succ) edgeIndex of
      Just idx -> makeEdgeBlockName pred succ idx

      -- Leave target unchanged if we're not splitting this edge. 
      Nothing -> return $ succ 
    fixPred pred succ = case Map.lookup (pred, succ) edgeIndex of
      Just idx -> makeEdgeBlockName pred succ idx

      -- Leave target unchanged if we're not splitting this edge. 
      Nothing -> return $ pred

-- | Removes phis. Assumes edge splitting has already happened.
removePhi :: (Rprog Metadata wrdT, Word) -> Hopefully (Rprog Metadata wrdT, Word)
removePhi (IRprog te ge funcs ext, nextReg) = do
  (funcs', nextReg') <- runStateT (mapM removePhiFunc funcs) nextReg
  return (IRprog te ge funcs' ext, nextReg')

removePhiFunc :: forall wrdT.
  RFunction Metadata wrdT -> StateT Word Hopefully (RFunction Metadata wrdT)
removePhiFunc f@(Function name retTy argTys argNms blocks extern) = do
    blocks' <- mapM removePhiBlock blocks
    return $ Function name retTy argTys argNms blocks' extern
  where
    -- Remove phis from a block.
    removePhiBlock :: BB Name (RTLInstr Metadata wrdT) -> StateT Word Hopefully (BB Name (RTLInstr Metadata wrdT))
    removePhiBlock block = do
      -- Remove phis.
      let (BB name body term dag) = removePhis block

      -- Append moves.
      moves <- case Map.lookup name phiMoves of
        Nothing -> return []
        Just succPhiMoves -> case Map.toList succPhiMoves of
          [] -> return []
          [succPhiMoves] ->
            generateMoves $ snd succPhiMoves
          _ ->
            lift $ assumptError "removePhiFunc: More than one succesor block."

      return $ BB name (body <> moves) term dag


    generateMoves :: [(Name, MAOperand Name wrdT, Metadata)] -> StateT Word Hopefully [RTLInstr Metadata wrdT]
    generateMoves ms = do
      (revBody, revPreBody, _) <- foldM (\(body, preBody, writtenRegs) (dest, op, mdata) -> do
        -- Generate an `Imov`, and record that `dest` was written.  However, if
        -- `op` reads from a previously written register, then we have to store
        -- the value to a temporary first, then read from the temporary later.
        case op of
          AReg r | Set.member r writtenRegs -> do
            tmpReg <- Name <$> get <* modify (+1) <*> return "tempReg"
            let move = MRI (Imov dest (AReg tmpReg)) mdata
            let preMove = MRI (Imov tmpReg op) mdata
            return (move : body, preMove : preBody, Set.insert dest writtenRegs)
          _ -> do
            let move = MRI (Imov dest op) mdata
            return (move : body, preBody, Set.insert dest writtenRegs)
        ) ([], [], Set.empty) ms
      return $ reverse revPreBody ++ reverse revBody
      

    -- Collect information about phi nodes in the function.

    -- | Map `predecessor -> successor -> assignments`.
    phiMoves :: Map Name (Map Name [(VReg, MAOperand VReg wrdT, Metadata)])
    phiMoves = funcPhiMoves f

-- | Gather all outgoing CFG edges from a block.  This considers only direct
-- jumps to labels - indirect jumps are ignored.
blockEdges :: BB Name (RTLInstr Metadata wrdT) -> Set (Name, Name)
blockEdges (BB name _body term _dag) = Set.unions $ map go term
  where
    go (MRI (Ijmp (Label dest)) _) = Set.singleton (name, dest)
    go (MRI (Icjmp _ (Label dest)) _) = Set.singleton (name, dest)
    go (MRI (Icnjmp _ (Label dest)) _) = Set.singleton (name, dest)
    go _ = Set.empty

_funcEdges :: RFunction Metadata wrdT -> Set (Name, Name)
_funcEdges f = Set.unions $ map blockEdges $ funcBlocks f



funcPhiMoves :: RFunction Metadata wrdT ->
  Map Name (Map Name [(VReg, MAOperand VReg wrdT, Metadata)])
funcPhiMoves f = foldr blockPhiMoves Map.empty $ funcBlocks f

blockPhiMoves :: BB Name (RTLInstr Metadata wrdT) ->
  Map Name (Map Name [(VReg, MAOperand VReg wrdT, Metadata)]) ->
  Map Name (Map Name [(VReg, MAOperand VReg wrdT, Metadata)])
blockPhiMoves (BB name body _term _dag) acc = foldr goInstr acc body
  where
    goInstr (IRI (RPhi dest srcs) mdata) m = foldr (goSrc dest mdata) m srcs
    goInstr _ m = m

    -- | Add the entry `label -> (dest, op)` to `m`, prepending if there are
    -- already some entries for `label` in `m`.
    goSrc dest mdata (op, predLabel) m =
      let d = (dest, op, mdata) in
      Map.alter (\case
          Nothing -> Just $ Map.singleton name [d]
          Just predM -> Just $ Map.alter (\old -> Just $ ((dest, op, mdata):) $ maybe [] id old) name predM
        ) predLabel m


makeEdgeBlockName :: Name -> Name -> Int -> State NameState Name
makeEdgeBlockName a b i = do
  NameState nextReg nMap <- get
  case Map.lookup (a,b) nMap of
    Just name' -> return name'
    Nothing -> do
      let name' = Name nextReg $ "phi" <> (fromString $ show i) <> "_" <> dbName a <> "_" <> dbName b
      put $ NameState (nextReg + 1) (Map.insert (a,b) name' nMap)
      return name'

removePhis :: BB Name (RTLInstr Metadata wrdT) -> BB Name (RTLInstr Metadata wrdT)
removePhis (BB name body term dag) = BB name (filter (not . isPhi) body) term dag
  where
    isPhi (IRI (RPhi _ _) _) = True
    isPhi _ = False
