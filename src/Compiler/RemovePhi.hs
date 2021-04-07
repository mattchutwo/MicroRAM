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


-- Split edges to ensure the "unique successor or predecessor property" (Modern Compiler Implementation in Java, 19.6).
edgeSplit :: Rprog Metadata wrdT -> Hopefully (Rprog Metadata wrdT)
edgeSplit (IRprog te ge funcs) = return $ IRprog te ge $ map edgeSplitFunc funcs

edgeSplitFunc :: forall wrdT . RFunction Metadata wrdT -> RFunction Metadata wrdT
edgeSplitFunc (Function name retTy argTys blocks nextReg) =
  let edgeBlocks = map (buildEdgeBlock name) $ Map.toList edgeIndex in
  Function name retTy argTys (blocks' ++ edgeBlocks) nextReg

  where
    cfg = buildCFG blocks

    -- Build new blocks to place along the previous CFG edges.

    buildEdgeBlock :: Name -> ((Name, Name), Int) -> BB Name (RTLInstr Metadata wrdT)
    buildEdgeBlock funcName ((pred, succ), edgeIdx) = 
      let name = makeEdgeBlockName pred succ edgeIdx in
      let body = [] in
      let md = trivialMetadata (show funcName) (show name) in
      let term = [MRI (Ijmp $ nameLabel succ) md] in -- This will eventually be deleted
      BB name body term [succ]

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
                let succs = Digraph.successors cfg nid in

                -- Skip if less than 2 successors.
                if List.length succs < 2 then
                  mempty
                else
                  Set.unions $ map (\succId -> 
                      -- Get predecessors of each successor.
                      let succPreds = Digraph.predecessors cfg succId in

                      -- Include if more than one predecessor.
                      if List.length succPreds > 1 then
                        Set.singleton (nid, succId)
                      else
                        mempty
                    ) succs
              ) ns
      in

      Map.fromList $ zip splitEdges [0..]

    -- Modify existing blocks

    blocks' = map fixInstrs blocks

    fixInstrs :: BB Name (RTLInstr Metadata wrdT) -> BB Name (RTLInstr Metadata wrdT)
    fixInstrs (BB name body term dag) = BB name (map (fixInstr name) body) (map (fixInstr name) term) (fixDag name dag)

    fixInstr :: Name -> RTLInstr Metadata wrdT -> RTLInstr Metadata wrdT
    fixInstr pred (MRI (Ijmp (Label succ)) mdata) = MRI (Ijmp (jumpDest pred succ)) mdata
    fixInstr pred (MRI (Icjmp r (Label succ)) mdata) = MRI (Icjmp r (jumpDest pred succ)) mdata
    fixInstr pred (MRI (Icnjmp r (Label succ)) mdata) = MRI (Icnjmp r (jumpDest pred succ)) mdata
    fixInstr succ (IRI (RPhi r vs) mdata) = IRI (RPhi r $ map (\(o,pred) -> (o, fixPred pred succ)) vs) mdata
    fixInstr _ instr = instr

    jumpDest :: Name -> String -> MAOperand VReg wrdT
    jumpDest pred succStr = nameLabel $ fixSucc pred (read succStr)

    fixDag pred succs = map (fixSucc pred) succs

    fixSucc pred succ = case Map.lookup (pred, succ) edgeIndex of
      Just idx -> makeEdgeBlockName pred succ idx

      -- Leave target unchanged if we're not splitting this edge. 
      Nothing -> succ 
    fixPred pred succ = case Map.lookup (pred, succ) edgeIndex of
      Just idx -> makeEdgeBlockName pred succ idx

      -- Leave target unchanged if we're not splitting this edge. 
      Nothing -> pred

-- | Removes phis. Assumes edge splitting has already happened.
removePhi :: Rprog Metadata wrdT -> Hopefully (Rprog Metadata wrdT)
removePhi (IRprog te ge funcs) = IRprog te ge <$> mapM removePhiFunc funcs

removePhiFunc :: forall wrdT.
  RFunction Metadata wrdT -> Hopefully (RFunction Metadata wrdT)
removePhiFunc f@(Function name retTy argTys blocks nextReg) = do
    (blocks', nextReg') <- runStateT (mapM removePhiBlock blocks) nextReg
    return $ Function name retTy argTys blocks' nextReg'
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
            tmpReg <- NewName <$> get <* modify (+1)
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

nameLabel :: Name -> MAOperand VReg wrdT
nameLabel n@(Name _) = Label $ show n
nameLabel (NewName w) = error $ "tried to convert NewName " ++ show w ++ " back to a label"

-- | Gather all outgoing CFG edges from a block.  This considers only direct
-- jumps to labels - indirect jumps are ignored.
blockEdges :: BB Name (RTLInstr Metadata wrdT) -> Set (Name, Name)
blockEdges (BB name _body term _dag) = Set.unions $ map go term
  where
    go (MRI (Ijmp (Label dest)) _) = Set.singleton (name, read dest)
    go (MRI (Icjmp _ (Label dest)) _) = Set.singleton (name, read dest)
    go (MRI (Icnjmp _ (Label dest)) _) = Set.singleton (name, read dest)
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


makeEdgeBlockName :: Name -> Name -> Int -> Name
makeEdgeBlockName a b i = Name $ "phi" <> (fromString $ show i) <> "_" <> go a <> "_" <> go b
  where
    go (Name n) = n
    go (NewName w) = fromString $ show w

removePhis :: BB Name (RTLInstr Metadata wrdT) -> BB Name (RTLInstr Metadata wrdT)
removePhis (BB name body term dag) = BB name (filter (not . isPhi) body) term dag
  where
    isPhi (IRI (RPhi _ _) _) = True
    isPhi _ = False
