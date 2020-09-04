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
import Data.Foldable
import qualified Data.Graph.Directed as Digraph
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)

import Compiler.IRs
import Compiler.Errors
import Compiler.RegisterAlloc.Liveness
import MicroRAM


-- Split edges to ensure the "unique successor or predecessor property" (Modern Compiler Implementation in Java, 19.6).
edgeSplit :: (Monoid mdata) => Rprog mdata wrdT -> Hopefully (Rprog mdata wrdT)
edgeSplit (IRprog te ge funcs) = IRprog te ge <$> mapM edgeSplitFunc funcs

edgeSplitFunc :: forall mdata wrdT . (Monoid mdata) => RFunction mdata wrdT -> Hopefully (RFunction mdata wrdT)
edgeSplitFunc (Function name retTy argTys blocks nextReg) = do
  (edgeBlocks, nextReg') <- runStateT (mapM buildEdgeBlock $ Map.toList edgeIndex) nextReg
  return $ Function name retTy argTys (blocks' ++ edgeBlocks) nextReg'

  where
    cfg = buildCFG blocks

    -- Build new blocks to place along the previous CFG edges.

    buildEdgeBlock :: ((Name, Name), Int) -> StateT Word Hopefully (BB Name (RTLInstr mdata wrdT))
    buildEdgeBlock ((pred, succ), edgeIdx) = do
      let name = makeEdgeBlockName pred succ edgeIdx
      let body = []
      let term = [MRI (Ijmp $ nameLabel succ) mempty]
      return $ BB name body term [succ]

    -- | Map from (pred, succ) to a unique index.  This is mainly used to
    -- ensure that every new block gets a unique name, even if there are
    -- collisions such as `Name "0"` vs. `NewName 0`.
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

    blocks' = map fixJumps blocks

    fixJumps :: BB Name (RTLInstr mdata wrdT) -> BB Name (RTLInstr mdata wrdT)
    fixJumps (BB name body term dag) = BB name (map (fixJump name) body) (map (fixJump name) term) (fixDag name dag)

    fixJump :: Name -> RTLInstr mdata wrdT -> RTLInstr mdata wrdT
    fixJump pred (MRI (Ijmp (Label succ)) mdata) = MRI (Ijmp (jumpDest pred succ)) mdata
    fixJump pred (MRI (Icjmp (Label succ)) mdata) = MRI (Icjmp (jumpDest pred succ)) mdata
    fixJump pred (MRI (Icnjmp (Label succ)) mdata) = MRI (Icnjmp (jumpDest pred succ)) mdata
    fixJump _ instr = instr

    jumpDest :: Name -> String -> MAOperand VReg wrdT
    jumpDest pred succStr = nameLabel $ fixSucc pred (read succStr)

    fixDag pred succs = map (fixSucc pred) succs

    fixSucc pred succ = case Map.lookup (pred, succ) edgeIndex of
      Just idx -> makeEdgeBlockName pred succ idx

      -- Leave target unchanged if we're not splitting this edge. 
      Nothing -> succ 

removePhi :: (Monoid mdata) => Rprog mdata wrdT -> Hopefully (Rprog mdata wrdT)
removePhi (IRprog te ge funcs) = IRprog te ge <$> mapM removePhiFunc funcs

removePhiFunc :: forall mdata wrdT.
  (Monoid mdata) =>
  RFunction mdata wrdT -> Hopefully (RFunction mdata wrdT)
removePhiFunc f@(Function name retTy argTys blocks nextReg) = do
    (edgeBlocks, nextReg') <- runStateT (mapM buildEdgeBlock $ Map.toList edgeIndex) nextReg
    return $ Function name retTy argTys (blocks' ++ edgeBlocks) nextReg'
  where
    -- Collect information about the CFG edges and phi nodes in the function.

    -- | List of all CFG edges.
    edges :: Seq (Name, Name)
    edges = Seq.fromList $ Set.toList $ funcEdges f
    -- | Map from (pred, succ) to its index in `edges`.  This is mainly used to
    -- ensure that every new block gets a unique name, even if there are
    -- collisions such as `Name "0"` vs. `NewName 0`.
    edgeIndex :: Map (Name, Name) Int
    edgeIndex = Map.fromList $ zip (toList edges) [0..]

    -- | Map `successor -> predecessor -> assignments`.
    phiMoves :: Map Name (Map Name [(VReg, MAOperand VReg wrdT, mdata)])
    phiMoves = funcPhiMoves f


    -- Modify existing blocks

    blocks' = map (fixJumps . removePhis) blocks

    fixJumps :: BB Name (RTLInstr mdata wrdT) -> BB Name (RTLInstr mdata wrdT)
    fixJumps (BB name body term dag) = BB name body (map (fixJump name) term) (fixDag name dag)

    fixJump :: Name -> RTLInstr mdata wrdT -> RTLInstr mdata wrdT
    fixJump pred (MRI (Ijmp (Label succ)) mdata) = MRI (Ijmp (jumpDest pred succ)) mdata
    fixJump pred (MRI (Icjmp (Label succ)) mdata) = MRI (Icjmp (jumpDest pred succ)) mdata
    fixJump pred (MRI (Icnjmp (Label succ)) mdata) = MRI (Icnjmp (jumpDest pred succ)) mdata
    fixJump _ instr = instr

    jumpDest :: Name -> String -> MAOperand VReg wrdT
    jumpDest pred succStr = nameLabel $ fixSucc pred (read succStr)

    fixDag pred succs = map (fixSucc pred) succs

    fixSucc pred succ = case Map.lookup (pred, succ) edgeIndex of
      Just idx -> makeEdgeBlockName pred succ idx
      Nothing -> error $ "no edge index for " ++ show (pred, succ)


    -- Build new blocks to place along the previous CFG edges.

    buildEdgeBlock :: ((Name, Name), Int) -> StateT Word Hopefully (BB Name (RTLInstr mdata wrdT))
    buildEdgeBlock ((pred, succ), edgeIdx) = do
      let name = makeEdgeBlockName pred succ edgeIdx
      let moves = maybe [] id $ Map.lookup succ phiMoves >>= Map.lookup pred
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
        ) ([], [], Set.empty) moves
      let body = reverse revPreBody ++ reverse revBody
      let term = [MRI (Ijmp $ nameLabel succ) mempty]
      return $ BB name body term [succ]

nameLabel :: Name -> MAOperand VReg wrdT
nameLabel n@(Name _) = Label $ show n
nameLabel (NewName w) = error $ "tried to convert NewName " ++ show w ++ " back to a label"

-- | Gather all outgoing CFG edges from a block.  This considers only direct
-- jumps to labels - indirect jumps are ignored.
blockEdges :: BB Name (RTLInstr mdata wrdT) -> Set (Name, Name)
blockEdges (BB name _body term _dag) = Set.unions $ map go term
  where
    go (MRI (Ijmp (Label dest)) _) = Set.singleton (name, read dest)
    go (MRI (Icjmp (Label dest)) _) = Set.singleton (name, read dest)
    go (MRI (Icnjmp (Label dest)) _) = Set.singleton (name, read dest)
    go _ = Set.empty

funcEdges :: RFunction mdata wrdT -> Set (Name, Name)
funcEdges f = Set.unions $ map blockEdges $ funcBlocks f



funcPhiMoves :: RFunction mdata wrdT ->
  Map Name (Map Name [(VReg, MAOperand VReg wrdT, mdata)])
funcPhiMoves f = Map.unions $ map blockPhiMoves $ funcBlocks f

blockPhiMoves :: BB Name (RTLInstr mdata wrdT) ->
  Map Name (Map Name [(VReg, MAOperand VReg wrdT, mdata)])
blockPhiMoves (BB name body _term _dag) = Map.singleton name $ foldr goInstr Map.empty body
  where
    goInstr (IRI (RPhi dest srcs) mdata) m = foldr (goSrc dest mdata) m srcs
    goInstr _ m = m

    -- | Add the entry `label -> (dest, op)` to `m`, prepending if there are
    -- already some entries for `label` in `m`.
    goSrc dest mdata (op, label) m =
      Map.alter (\old -> Just $ ((dest, op, mdata):) $ maybe [] id old) label m


makeEdgeBlockName :: Name -> Name -> Int -> Name
makeEdgeBlockName a b i = Name $ "phi" <> (fromString $ show i) <> "_" <> go a <> "_" <> go b
  where
    go (Name n) = n
    go (NewName w) = fromString $ show w

removePhis :: BB Name (RTLInstr mdata wrdT) -> BB Name (RTLInstr mdata wrdT)
removePhis (BB name body term dag) = BB name (filter (not . isPhi) body) term dag
  where
    isPhi (IRI (RPhi _ _) _) = True
    isPhi _ = False
