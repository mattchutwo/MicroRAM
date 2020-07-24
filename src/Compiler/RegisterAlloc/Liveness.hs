{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Compiler.RegisterAlloc.Liveness where

import           Data.Graph.Directed (DiGraph)
import qualified Data.Graph.Directed as DiGraph
import qualified Data.Queue as Queue
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Compiler.CompileErrors
import           Compiler.IRs
import           Compiler.RegisterAlloc.Internal

-- import Debug.Trace

-- If the target instruction is Nothing, it's a return edge.
type LivenessResult instname = Map (instname, Maybe instname) (Set VReg) -- Map VReg [(Loc, Loc)]

-- Assumes SSA.
-- A good resource: https://www.seas.upenn.edu/~cis341/current/lectures/lec22.pdf
livenessAnalysis :: Ord name -- => (Show name, Show wrdT, Show mdata) 
  => [BB name (LTLInstr mdata VReg wrdT)] 
  -> Hopefully (LivenessResult name)
livenessAnalysis blocks = do -- trace (show blocks) $ do

  -- Reverse topological sort the CFG.
  let vs = reverse $ DiGraph.topSort cfg

  -- Compute in and out for each block.
  -- go mempty mempty vs
  go $ Queue.fromList vs

  -- Build graph. 
  -- Just union of out and in for the edge in the graph??


  -- trace (show vs) $ error "TODO"

  where
    -- Build CFG (Edges of instruction names?)
    cfg = buildCFG blocks

    -- Compute use and define set for each block.
    (useM, defM) = foldr (\(BB name insts _) (useM, defM) ->
            -- let useM' = Map.insertWith Set.union name (Set.unions $ map readRegisters insts) useM in
            let useM' = Map.insert name (Set.unions $ map readRegisters insts) useM in
            let defM' = Map.insert name (Set.unions $ map writeRegisters insts) defM in

            (useM', defM')
          ) (mempty, mempty) blocks

    go q | Nothing <- Queue.pop q = error "TODO"
    go q = 
      -- TODO:
      --
      -- let instates[v] = (outstates[v] `union` readRegisters v) \ writeRegisters v
      -- 
      -- Check successors of v?
      -- If not visited, revisit v after?
    
      error "TODO"
    

buildCFG :: Ord name => [BB name inst] -> DiGraph name [inst] ()
buildCFG blocks = DiGraph.setNodeLabels nodeLabels $ DiGraph.fromEdges edges
  where
    edges = concatMap (\(BB name insts names) -> 
        map (name,) names
      ) blocks

    -- JP: We have the invariant that each block has one instruction, so we could return `DiGraph name inst ()` instead.
    nodeLabels = map (\(BB name insts names) -> (name, insts)) blocks

