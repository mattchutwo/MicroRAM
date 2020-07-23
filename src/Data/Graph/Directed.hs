{-# LANGUAGE TupleSections #-}
module Data.Graph.Directed (
    DiGraph
  , fromEdges
  , setNodeLabels
  , topSort
  , topSortWithLabels
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Graph.Haggle as HGL
import qualified Data.Graph.Haggle.Algorithms.DFS as HGL
import qualified Data.Graph.Haggle.VertexMap as HGL

-- Bidirectional graph with labelled nodes.
data DiGraph nid n e = DiGraph {
    diGraph :: HGL.VertexLabeledGraph HGL.Digraph nid
  , diGraphVertexKeys :: HGL.VertexMap nid
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
topSort' (DiGraph g l _) = mapM (HGL.vertexLabel g) $ HGL.topsort g

topSortWithLabels :: Ord nid => DiGraph nid n e -> [(nid, n)]
topSortWithLabels = maybe (error "topSort: Invalid DiGraph.") id . topSortWithLabels'

topSortWithLabels' :: Ord nid => DiGraph nid n e -> Maybe [(nid, n)]
topSortWithLabels' g = topSort' g >>= mapM (\nid -> (nid,) <$> Map.lookup nid (diGraphVertexLabel g))
