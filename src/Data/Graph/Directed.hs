{-# LANGUAGE TupleSections #-}

module Data.Graph.Directed (
    DiGraph
  , edges
  , fromEdges
  , nodes
  , predecessors
  , setNodeLabels
  , successors
  , topSort
  , topSortWithLabels
  ) where

import qualified Data.Graph.Haggle as HGL
import qualified Data.Graph.Haggle.Algorithms.DFS as HGL
import qualified Data.Graph.Haggle.VertexMap as HGL
import           Data.Map (Map)
import qualified Data.Map as Map

-- Bidirectional graph with labelled nodes.
data DiGraph nid n e = DiGraph {
    _diGraph :: HGL.VertexLabeledGraph HGL.Digraph nid
  , _diGraphVertexKeys :: HGL.VertexMap nid
  , diGraphVertexLabel :: Map nid n
  }

fromEdges :: Ord nid => [(nid, nid)] -> DiGraph nid () ()
fromEdges edges = DiGraph graph vmap mempty
  where
    (graph, vmap) = HGL.fromEdgeList HGL.newMDigraph edges

setNodeLabels :: Ord nid => [(nid, n)] -> DiGraph nid a e -> DiGraph nid n e
setNodeLabels labels g = g {diGraphVertexLabel = Map.fromList labels}

topSort :: DiGraph nid n e -> [nid]
topSort = maybe (error "topSort: Invalid DiGraph.") id . topSort'

topSort' :: DiGraph nid n e -> Maybe [nid]
topSort' (DiGraph g _ _) = mapM (HGL.vertexLabel g) $ HGL.topsort g

topSortWithLabels :: Ord nid => DiGraph nid n e -> [(nid, n)]
topSortWithLabels = maybe (error "topSort: Invalid DiGraph.") id . topSortWithLabels'

topSortWithLabels' :: Ord nid => DiGraph nid n e -> Maybe [(nid, n)]
topSortWithLabels' g = topSort' g >>= mapM (\nid -> (nid,) <$> Map.lookup nid (diGraphVertexLabel g))

-- mapVertexLabel :: (n -> n') -> DiGraph nid n e -> DiGraph nid n' e
-- mapVertexLabel f g = g { diGraphVertexLabel = fmap f (diGraphVertexLabel g) }

successors :: Ord nid => DiGraph nid n e -> nid -> [nid]
successors g nid = maybe (error "successors: Invalid DiGraph.") id $ successors' g nid

successors' :: Ord nid => DiGraph nid n e -> nid -> Maybe [nid]
successors' (DiGraph g l _) nid = HGL.lookupVertexForLabel nid l >>= mapM (HGL.vertexLabel g) . HGL.successors g

predecessors :: Ord nid => DiGraph nid n e -> nid -> [nid]
predecessors g nid = maybe (error "predecessors: Invalid DiGraph.") id $ predecessors' g nid

predecessors' :: Ord nid => DiGraph nid n e -> nid -> Maybe [nid]
predecessors' (DiGraph g l _) nid = HGL.lookupVertexForLabel nid l >>= mapM (HGL.vertexLabel g) . preds g
  where
    -- preds = HGL.predecessors -- JP: Digraph doesn't implement this?
    -- TODO: More efficient way to do this
    preds g v = map HGL.edgeSource $ filter (\e -> HGL.edgeDest e == v) $ HGL.edges g

nodes :: DiGraph nid n e -> [nid]
nodes = maybe (error "nodes: Invalid DiGraph.") id . nodes'

nodes' :: DiGraph nid n e -> Maybe [nid]
nodes' (DiGraph g _ _) = mapM (HGL.vertexLabel g) $ HGL.vertices g

edges :: DiGraph nid n e -> [(nid,nid)]
edges = maybe (error "edges: Invalid DiGraph.") id . edges'

edges' :: DiGraph nid n e -> Maybe [(nid,nid)]
edges' (DiGraph g _ _) = mapM (\e -> (,) <$> toNid (HGL.edgeSource e) <*> toNid (HGL.edgeDest e)) $ HGL.edges g
  where
    toNid = HGL.vertexLabel g

