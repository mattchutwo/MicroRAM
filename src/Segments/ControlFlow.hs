{-
Module      : Control flow analysis
Description : 
Maintainer  : santiago@galois.com
Stability   : experimental

Control-flow analysis for MicroRAM programs.  Includes functions for dividing a
MicroRAM program into blocks, building a CFG, and computing dominator and
postdominator trees.

Relies on metadata for precise handling of indirect jumps.
-}
module Segments.ControlFlow (
  ProgramCFG(..),
  buildProgramCFG
) where

import qualified Data.Graph.Dom as Dom
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (foldl')

import Compiler.IRs
import Compiler.Metadata
import MicroRAM


-- | Control-flow data for a fully lowered MicroRAM program.
--
-- `ProgramCFG` uses `Int` instead of `MWord` for instruction indices / program
-- counter values, so that we can use `IntMap` and `IntSet` for representing
-- the graphs.  The special value `returnNode` (a.k.a `-1 :: Int`) is used to
-- indicate function returns.
data ProgramCFG r = ProgramCFG {
  -- | Program counter values for the starts of all basic blocks.
  _pBlockStarts :: IntSet,
  -- | Maps each basic block to the entry point of its containing function.
  _pBlockFuncs :: IntMap Int,
  -- | Successors of each block.
  _pSuccs :: IntMap IntSet,
  -- | Predecessors of each block.
  _pPreds :: IntMap IntSet,
  -- | Immediate dominator of each block.
  _pIdoms :: IntMap Int,
  -- | Immediate postdominator of each block.
  _pIpdom :: IntMap Int
}
  deriving (Show)

-- | Special program counter value indicating the return address of the current
-- function.  Indirect jumps that are used to return are treated as jumps to
-- `returnNode`.
returnNode :: Int
returnNode = -1

-- | Special program counter value indicating program termination.  The
-- `Ianswer` instruction is treated as a jump to `exitNode`.  This is only used
-- internally - it should never appear in the final `ProgramCFG`.  We use this
-- only for marking instructions that have no successor, not even the implicit
-- successor of `pc + 1`.
exitNode :: Int
exitNode = -2


isCondJump :: Instruction r MWord -> Bool
isCondJump (Icjmp _ _) = True
isCondJump (Icnjmp _ _) = True
isCondJump _ = False

-- | Find all the jumps that can occur in the program and returns a list giving
-- `(source, dest)` for each one.  This counts only intraprocedural jumps -
-- function calls and returns are not included in the list.
gatherJumps :: AnnotatedProgram Metadata r MWord -> [(Int, Int)]
gatherJumps prog = concatMap go $ zip [0..] prog
  where
    go (pc, (i, meta))
      | mdIsCall meta = case i of
        -- After the function call finishes, control resumes at the next
        -- instruction.  We don't create an edge from `pc` to the callee, since
        -- we are only computing per-function CFGs (even though we run the
        -- analysis over the whole program at once).
        Ijmp (Const _) -> [(pc, pc + 1)]
        Ijmp (Reg _) -> [(pc, pc + 1)]
        _ | isCondJump i -> error $
          "impossible: conditional jump at " ++ show pc ++ " is marked with call metadata"
        -- Some passes might add extra instructions around a call, copying the
        -- metadata with `mdIsCall` still set.  This is okay as long as those
        -- instructions don't affect control flow.
        _ -> []

      | mdIsReturn meta = case i of
        Ijmp (Reg _) -> [(pc, -1)]
        Ijmp (Const _) -> error $
          "impossible: direct jump at " ++ show pc ++ " is marked with return metadata"
        _ | isCondJump i -> error $
          "impossible: conditional jump at " ++ show pc ++ " is marked with return metadata"
        _ -> []

      | otherwise = case i of
        Ijmp (Const pc') -> [(pc, fromIntegral pc')]
        Icjmp _ (Const pc') -> [(pc, fromIntegral pc'), (pc, pc + 1)]
        Icnjmp _ (Const pc') -> [(pc, fromIntegral pc'), (pc, pc + 1)]
        -- Currently, the only source of indirect jumps in
        -- `InstructionSelection` is indirect calls, which are handled above.
        -- If we ever implement jump tables for switches, we'll need to add a
        -- new case above using appropriate metadata.
        _ | isCondJump i -> error $
          "impossible: found conditional indirect jump at " ++ show pc

        Ianswer _ -> [(pc, exitNode)]
        -- TODO: would be nice to mark __cc_flag_invalid with exitNode as well

        _ -> []

-- | Get the start PC of every basic block in the program.
findBlockStarts :: IntSet -> [(Int, Int)] -> IntSet
findBlockStarts funcStarts jumps =
  IntSet.unions [funcStarts, jumpTargets, IntSet.singleton 0]
  where
    -- The jump origins aren't the starts of blocks, and the following
    -- instruction might not be either (we allow for some instructions to be
    -- dead code, not included in the CFG).
    jumpTargets = IntSet.fromList $ filter (/= exitNode) $ map snd jumps

-- | Build the complete control-flow graph.  
buildGraph :: IntSet -> [(Int, Int)] -> IntMap IntSet
buildGraph blks jumps = go mempty jumps
  where 
    go acc [] = acc
    go acc ((src, dest) : jumps)
      -- Jumps to the dummy `exitNode` are used to indicate blocks that have no
      -- successors.
      | dest == exitNode = go acc jumps
      | otherwise = go acc' jumps
      where
        -- Find the block containing `src`.
        srcBlock = case IntSet.lookupLE src blks of
          Just x -> x
          -- This should never happen because all jump sources are real PC
          -- values (not special markers like `returnNode`) and 
          Nothing -> error $ "impossible: jump source " ++ show src ++ " is not in any block"
          
        acc' = IntMap.insertWith IntSet.union srcBlock (IntSet.singleton dest) acc

-- | Reverse all edges in the graph, converting a map that gives successors
-- into one that gives predecessors instead.
flipGraph :: IntMap IntSet -> IntMap IntSet
flipGraph g = go mempty [(a, b) | (a, bs) <- IntMap.toList g, b <- IntSet.toList bs]
  where
    go acc [] = acc
    go acc ((a, b) : edges) = go acc' edges
      where
        acc' = IntMap.insertWith IntSet.union b (IntSet.singleton a) acc

-- | Collect all nodes reachable from `n` by following the edges of `g`.
gatherReachable :: IntMap IntSet -> Int -> IntSet
gatherReachable g n = go (IntSet.singleton n) (IntSet.empty)
  where
    go pending visited
      | Just (cur, pending') <- IntSet.maxView pending =
        -- Mark `cur` as visited.
        let visited' = IntSet.insert cur visited
            -- Mark any unvisited successors of `cur` as pending.
            succs = maybe IntSet.empty id $ IntMap.lookup cur g
            new = IntSet.difference succs visited'
            pending'' = IntSet.union pending' new
        in go pending'' visited'
      | otherwise = visited

-- | Calculate the immediate dominator (`idom`) and immediate postdominator
-- (`ipdom`) maps for a particular function.  `blks` is the set of blocks that
-- makes up the function.
calcDominators :: IntMap IntSet -> Int -> IntSet -> (IntMap Int, IntMap Int)
calcDominators g entry blks = (idom, ipdom)
  where
    -- The `dom-lt` (`Data.Graph.Dom`) library is very finicky, and will
    -- segfault if you use it wrong.  It seems to require the node IDs to be
    -- sequential integers starting from 0, and it only produces correct
    -- results if every node has an entry in the `IntMap`, even if the node has
    -- no successors.

    -- We begin by compacting the block IDs, constructing a mapping from `blks`
    -- to `[0 .. size blks - 1]`.  We also build the reverse mapping so the
    -- results can be mapped back to the original IDs.
    allIDs = IntSet.toList $ IntSet.insert returnNode $ blks
    toCompactMap = IntMap.fromList $ zip allIDs [0..]
    fromCompactMap = IntMap.fromList $ zip [0..] allIDs

    -- Build the graph to be passed to the `Dom` library.  We build it by
    -- extracting the elements from `g` that are mentioned by `blks` and
    -- replacing all IDs with the compact ones.
    domG = IntMap.map extract fromCompactMap
    extract oldID = newSuccs
      where
        -- Provide an empty `IntSet` for blocks with no successors, instead of
        -- omitting the entry.  If the entry is omitted here then there will
        -- also be no entry in the output of `Dom.idom`.
        oldSuccs = maybe IntSet.empty id $ IntMap.lookup oldID g
        newSuccs = IntSet.map toCompact oldSuccs

        toCompact n = case IntMap.lookup n toCompactMap of
          Just x -> x
          Nothing -> error $ "impossible: block " ++ show oldID ++
            " in function " ++ show entry ++ " can jump to block " ++ show n ++
            ", which is not in the same function (missing from blks)"


    -- Compute the dominator and postdominator maps.
    fromCompact n = case IntMap.lookup n fromCompactMap of
      Just x -> x
      Nothing -> error $ "impossible: missing entry for " ++ show n ++ " in fromCompactMap"

    entryCompact = case IntMap.lookup entry toCompactMap of
      Just x -> x
      Nothing -> error $ "impossible: function entry block " ++ show entry ++
        " is missing from toCompactMap"
    idomRaw = Dom.idom (entryCompact, domG)
    -- We delete `returnNode` here since otherwise it will appear to be shared
    -- between multiple functions, causing an error.
    idom = IntMap.delete returnNode $
      IntMap.fromList $ map (\(a, b) -> (fromCompact a, fromCompact b)) idomRaw

    -- `ipdom` requires the return node as the graph root instead of the entry node.
    returnCompact = case IntMap.lookup returnNode toCompactMap of
      Just x -> x
      Nothing -> error $ "impossible: function return block " ++ show returnNode ++
        " is missing from toCompactMap"
    ipdomRaw = Dom.ipdom (returnCompact, domG)
    ipdom = IntMap.delete returnNode $
      IntMap.fromList $ map (\(a, b) -> (fromCompact a, fromCompact b)) ipdomRaw

buildProgramCFG :: AnnotatedProgram Metadata r MWord -> ProgramCFG r
buildProgramCFG prog = ProgramCFG blockStarts blockFuncs succs preds idoms ipdoms
  where
    jumps = gatherJumps prog

    funcStartsList = [pc | (pc, (_, meta)) <- zip [0..] prog, mdFunctionStart meta]
    funcStarts = IntSet.fromList funcStartsList
    blockStarts = findBlockStarts funcStarts jumps

    succs = buildGraph blockStarts jumps
    preds = flipGraph succs

    funcBlocks = IntMap.fromList [(entry, gatherReachable succs entry) | entry <- funcStartsList]
    blockFuncs = IntMap.mapWithKey f $ IntMap.delete returnNode $ flipGraph funcBlocks
      where
        f blk funcs
          | IntSet.size funcs == 1 = IntSet.findMin funcs
          | otherwise = error $ "impossible: block " ++ show blk ++
            " is present in multiple functions: " ++ show funcs

    funcIdomIpdomList = [calcDominators succs entry (getBlocks entry) | entry <- funcStartsList]
      where
        getBlocks n = case IntMap.lookup n funcBlocks of
          Just x -> x
          Nothing -> error $ "impossible: function " ++ show n ++ " has no blocks"
    funcIdomsList = map fst funcIdomIpdomList
    funcIpdomsList = map snd funcIdomIpdomList

    idoms = foldl'
      (IntMap.unionWithKey
        (\node dom1 dom2 -> error $
          "impossible: node " ++ show node ++ " is a member of two different functions; " ++
            "in one it has idom " ++ show dom1 ++ ", and in the other it has idom " ++ show dom2))
      IntMap.empty funcIdomsList

    ipdoms = foldl'
      (IntMap.unionWithKey
        (\node dom1 dom2 -> error $
          "impossible: node " ++ show node ++ " is a member of two different functions; " ++
            "in one it has ipdom " ++ show dom1 ++ ", and in the other it has ipdom " ++ show dom2))
      IntMap.empty funcIpdomsList
