{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Compiler.RegisterAlloc.Liveness where

import           Data.Graph.Directed (DiGraph)
import qualified Data.Graph.Directed as DiGraph
import           Data.Map (Map)
import           Data.Set (Set)

import           Compiler.CompileErrors
import           Compiler.IRs

import Debug.Trace

-- If the target instruction is Nothing, it's a return edge.
type LivenessResult instname = Map (instname, Maybe instname) (Set VReg) -- Map VReg [(Loc, Loc)]

livenessAnalysis :: Ord name => (Show name, Show wrdT, Show mdata) 
  => [BB name (LTLInstr mdata VReg wrdT)] 
  -> Hopefully (LivenessResult name)
livenessAnalysis blocks = do -- trace (show blocks) $ do
  -- Build CFG (Edges of instruction names?)
  let cfg = buildCFG blocks

  -- Reverse topological sort the CFG.
  let vs = reverse $ DiGraph.topSortWithLabels cfg


  trace (show vs) $ error "TODO"

buildCFG :: Ord name => [BB name inst] -> DiGraph name inst ()
buildCFG blocks = DiGraph.setNodeLabels nodeLabels $ DiGraph.fromEdges edges
  where
    edges = concatMap (\(BB name insts names) -> 
        map (name,) names
      ) blocks

    nodeLabels = map (\case
        BB name [inst] names -> (name, inst)
        -- JP: We can eliminate this requirement by returning a `DiGraph name [inst] ()` instead.
        _ -> error "Invariant: There should only be one instruction per basic block."
      ) blocks
