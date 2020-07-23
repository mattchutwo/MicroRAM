module Data.Graph.Undirected where

import           Data.Map (Map)

type Graph n e = ()

fromEdges :: [(n,n,e)] -> Graph n e
fromEdges = error "TODO"


color :: [color] -> [n] -> Graph n e -> Either n (Map n color)
color colors sortedNodes graph = error "TODO"

