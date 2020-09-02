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
    ( removePhi
    ) where

import Control.Monad.State
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)

import Compiler.IRs
import Compiler.Errors
import MicroRAM

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
      let body = map (\(dest, op, mdata) -> MRI (Imov dest op) mdata) moves
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
