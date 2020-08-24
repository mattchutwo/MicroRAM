{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Undirected (
    Graph
  , color
  , fromEdges
  , insertVertex
  ) where

import qualified Data.Graph.Haggle as HGL
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Graph nid e = Graph {
    graph :: HGL.PatriciaTree nid e -- SimpleBiDigraph?
  , graphVertexMap :: Map nid HGL.Vertex
  }

insertVertex :: Ord n => n -> Graph n e -> Graph n e
insertVertex n g@(Graph _ vm) | Map.member n vm = g
insertVertex n (Graph g vm) = 
  let (v, g') = HGL.insertLabeledVertex g n in
  let vm' = Map.insert n v vm in
  Graph g' vm'

fromEdges :: forall n e . Show e => Show n => Ord n => [(n,n,e)] -> Graph n e
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


color :: forall color n e . (Ord color, Ord n, Show n) => [color] -> Graph n e -> Either n (Map n color)
color colors' (Graph graph vmap) = 
  -- TODO Sort nodes by some priority for spilling.
  let sortedNodes = map (vertexToNode graph) $ HGL.vertices graph in
  go sortedNodes graph

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
          let graph' = maybe id (flip HGL.deleteVertex) (nodeToVertexM n) graph in

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


    -- vertexToNodeM g v = HGL.vertexLabel g v

    vertexToNode :: HGL.PatriciaTree n e -> HGL.Vertex -> n
    vertexToNode g v = case HGL.vertexLabel g v of
      Nothing -> error "color: Invalid Graph."
      Just n -> n

    nodeToVertexM n = Map.lookup n vmap

    -- nodeToVertex n = case nodeToVertexM n of
    --   Nothing -> error $ "color: Invalid Graph." ++ show n
    --   Just v -> v

    findColor :: n -> HGL.PatriciaTree n e -> Map n color -> Maybe color
    findColor n graph coloring = 
      -- Get neighbors.
      let neighbors = maybe [] (\v -> HGL.successors graph v ++ HGL.predecessors graph v) $ nodeToVertexM n in

      -- Convert to their colorings.
      let usedColors = Set.fromList $ maybe (error "color: Unknown coloring.") id $ mapM (flip Map.lookup coloring . vertexToNode graph) neighbors in

      -- Choose a remaining color.
      let available = colors `Set.difference` usedColors in

      case Set.toList available of
        (c:_) -> Just c
        []    -> Nothing


    -- Find a node with less than k edges.
    findNode []     g = Nothing
    findNode (n:ns) g = case nodeToVertexM n of
      Nothing ->
        -- Not in graph, so it has less than k edges.
        Just (n, ns)
      Just v ->
        if length (HGL.successors g v) < k then
          Just (n, ns)
        else
          (\(n',ns) -> (n', n:ns)) <$> findNode ns g

    spillNode (h:_) = Left h
    spillNode _     = error "color: No nodes available to spill."

    colors = Set.fromList colors'
    
    k = length colors'

