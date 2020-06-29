module Data.Graph.Undirected where

import           Data.Map (Map)
import qualified Data.Graph.Inductive.Graph as Graph
import           Data.Graph.Inductive.PatriciaTree (Gr)

data Graph n e = Graph

fromEdges :: [(n,n,e)] -> Graph n e
fromEdges = error "TODO"


color :: [color] -> [n] -> Graph n e -> Either (Map n color) n
color colors sortedNodes graph = error "TODO"

