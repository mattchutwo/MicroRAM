{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Undirected (
    Graph
  , fromEdges
  , color
  ) where

import qualified Data.Graph.Haggle as HGL
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Graph nid e = Graph {
    graph :: HGL.PatriciaTree nid e -- SimpleBiDigraph?
  , graphVertexMap :: Map nid HGL.Vertex
  }

fromEdges :: forall n e . Ord n => [(n,n,e)] -> Graph n e
fromEdges edges = Graph graph vmap

  where
    (graph, vmap) = foldr (\(n1,n2,e) g ->
        insertEdge n1 n2 e $ insertNode n2 $ insertNode n1 g
      ) (HGL.emptyGraph, mempty) edges

    insertNode :: n -> (HGL.PatriciaTree n e, Map n HGL.Vertex) -> (HGL.PatriciaTree n e, Map n HGL.Vertex)
    insertNode n (g, vmap) = case Map.lookup n vmap of
      Nothing ->
        let (v, g') = HGL.insertLabeledVertex g n in
        let vmap' = Map.insert n v vmap in
        (g', vmap')
      Just _v -> 
        (g, vmap)

    insertEdge n1 n2 e (g, vmap)
      | Just v1 <- Map.lookup n1 vmap
      , Just v2 <- Map.lookup n2 vmap = case HGL.insertLabeledEdge g v1 v2 e of
        Nothing       -> (g, vmap)
        Just (_e, g') -> (g', vmap)

      | otherwise = error "fromEdges: Invalid Graph."


-- All nodes in graph must be in `sortedNodes`.
color :: forall color n e . (Ord color, Ord n) => [color] -> [n] -> Graph n e -> Either n (Map n color)
color colors' sortedNodes (Graph graph vmap) = go sortedNodes graph
  where
    go :: [n] -> HGL.PatriciaTree n e -> Either n (Map n color)
    go sortedNodes graph | HGL.isEmpty graph = Right mempty
    go sortedNodes graph =
      -- Find a node with less than k edges.
      case findNode sortedNodes graph of
        Nothing ->
          -- If one doesn't exist, spill a node.
          spillNode sortedNodes
        Just (n, sortedNodes') ->
          -- Remove it.
          let graph' = HGL.deleteVertex graph $ nodeToVertex n in

          -- Recursive call.
          case go sortedNodes' graph' of
            spill@(Left _) -> spill
            Right coloring ->
              -- Find a color for the node.
              case findColor n graph coloring of
                Nothing ->
                  -- JP: Should be impossible?
                  Left n
                Just c ->
                  -- Assign the color.
                  Right $ Map.insert n c coloring


    vertexToNode :: HGL.PatriciaTree n e -> HGL.Vertex -> n
    vertexToNode g v = case HGL.vertexLabel g v of
      Nothing -> error "color: Invalid Graph."
      Just n -> n

    nodeToVertex n = case Map.lookup n vmap of
      Nothing -> error "color: Invalid Graph."
      Just v -> v

    findColor :: n -> HGL.PatriciaTree n e -> Map n color -> Maybe color
    findColor n graph coloring = 
      -- Get neighbors.
      let neighbors = HGL.successors graph $ nodeToVertex n in

      -- Convert to their colorings.
      let usedColors = Set.fromList $ maybe (error "color: Unknown coloring.") id $ mapM (flip Map.lookup coloring . vertexToNode graph) neighbors in

      -- Choose a remaining color.
      let available = colors `Set.difference` usedColors in

      case Set.toList available of
        (c:_) -> Just c
        []    -> Nothing


    findNode []     g = Nothing
    findNode (n:ns) g = if length (HGL.successors g $ nodeToVertex n) < k then
        Just (n, ns)
      else
        (\(n',ns) -> (n', n:ns)) <$> findNode ns g

    spillNode (h:_) = Left h
    spillNode _     = error "color: No nodes available to spill."

    colors = Set.fromList colors'
    
    k = length colors'

