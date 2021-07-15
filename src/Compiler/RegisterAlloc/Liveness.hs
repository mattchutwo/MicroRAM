{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Compiler.RegisterAlloc.Liveness where

import           Data.Graph.Directed (DiGraph)
import qualified Data.Graph.Directed as DiGraph
import qualified Data.Queue as Queue
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Compiler.Errors
import           Compiler.IRs
import           Compiler.RegisterAlloc.Internal

-- If the target instruction is Nothing, it's a return edge.
type LivenessResult instname reg = Map (instname, instname) (Set reg) -- Could make this a Graph

-- A good resource: https://www.seas.upenn.edu/~cis341/current/lectures/lec22.pdf
livenessAnalysis :: (Eq reg, Ord reg, Ord name) -- => (Show name, Show wrdT, Show mdata) 
  => [BB name (LTLInstr mdata reg wrdT)] 
  -> Hopefully (LivenessResult name reg)
livenessAnalysis blocks = do -- trace (show blocks) $ do

  -- Reverse topological sort the CFG.
  let vs = reverse $ DiGraph.topSort cfg

  -- Compute in and out for each block.
  let (ins, outs) = go mempty mempty $ Queue.fromList vs

  -- Build graph. 
  -- Just intersection of out and in for the edge in the graph??
  return $ foldr (\(v1, v2) acc -> 
      let v1out = maybe mempty id $ Map.lookup v1 outs in
      let v2in = maybe mempty id $ Map.lookup v2 ins in
      let regs = Set.intersection v1out v2in in
      let regs' = Set.union (lookupSet v1 defM) regs in -- Include any registers defined in v1.

      Map.insert (v1, v2) regs' acc
    ) mempty $ DiGraph.edges cfg

  where
    -- Build CFG (Edges of instruction names?)
    (cfg, predecessorMap, successroMap) = buildCFG blocks

    -- Compute use and define set for each block.
    (useM, defM) = foldr (\(BB name insts insts' _) (useM, defM) ->
            -- let useM' = Map.insertWith Set.union name (Set.unions $ map readRegisters insts) useM in
            let useM' = Map.insert name (Set.unions $ map readRegisters (insts' ++ insts)) useM in
            let defM' = Map.insert name (Set.unions $ map writeRegisters (insts' ++ insts)) defM in

            (useM', defM')
          ) (mempty, mempty) blocks

    lookupSet k = maybe mempty id . Map.lookup k

    getPredecessors v = maybe [] id $ Map.lookup v predecessorMap
    getSuccessors v = maybe [] id $ Map.lookup v successroMap

    go ins outs q = case Queue.pop q of
      Nothing -> (ins, outs)
      Just (q, v) -> 
        let oldIn = lookupSet v ins in

        let newOut = Set.unions $ map (\v' -> lookupSet v' ins) $ getSuccessors v in
        let outs' = Map.insert v newOut outs in

        let newIn = lookupSet v useM `Set.union` (newOut `Set.difference` lookupSet v defM) in
        let ins' = Map.insert v newIn ins in

        let q' = if oldIn /= newIn then foldr Queue.push q (getPredecessors v) else q in

        go ins' outs' q'

     
    
type DecessorMap name = Map.Map name [name]
  
buildCFG :: Ord name => [BB name inst] -> (DiGraph name ([inst], [inst]) (), DecessorMap name, DecessorMap name)
buildCFG blocks = (DiGraph.setNodeLabels nodeLabels $ DiGraph.fromEdges edges, predecessors, successors)
  where
    edges = concatMap (\(BB name _insts _insts' names) -> 
        map (name,) names
      ) blocks

    -- Compute predecessors and successsors
    (predecessors, successors) = foldl' (\(predMap, sucMap) (source,target) -> (addEdge target source predMap, addEdge source target sucMap))  (Map.empty, Map.empty) edges 
      where addEdge :: Ord name => name -> name -> DecessorMap name -> DecessorMap name
            addEdge key val pmap = let preds = maybe mempty id $ Map.lookup key pmap in
              Map.insert key (val:preds) pmap 
    -- JP: We have the invariant that each block has one instruction, so we could return `DiGraph name inst ()` instead.
    nodeLabels = map (\(BB name insts insts' _names) -> (name, (insts, insts'))) blocks

