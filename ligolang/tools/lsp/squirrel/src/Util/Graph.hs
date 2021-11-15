{-# LANGUAGE OverloadedLists #-}

module Util.Graph
  ( traverseAM
  , traverseAMConcurrently
  , wcc
  ) where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as G
import Control.Arrow ((&&&), second)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.State
import Data.Bool (bool)
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Foldable (for_, toList)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Tuple (swap)
import UnliftIO.Async (pooledMapConcurrently)

traverseAMImpl
  :: (Monad m, Ord a, Ord b)
  => ((a -> m (a, b)) -> [a] -> m [(a, b)])
  -> (a -> m b)
  -> AdjacencyMap a
  -> m (AdjacencyMap b)
traverseAMImpl traverser f g = do
  keysList <- traverser (sequenceA . (id &&& f)) (G.vertexList g)
  let adj = G.adjacencyMap g
  let keys = Map.fromList keysList
  pure $ G.fromAdjacencySets $ map (second (Set.map (keys Map.!) . (adj Map.!)) . swap) keysList

-- | Traverse an adjacency map.
traverseAM :: (Monad m, Ord a, Ord b) => (a -> m b) -> AdjacencyMap a -> m (AdjacencyMap b)
traverseAM = traverseAMImpl traverse

-- | Traverse an adjacency map concurrently.
traverseAMConcurrently :: (MonadUnliftIO m, Ord a, Ord b) => (a -> m b) -> AdjacencyMap a -> m (AdjacencyMap b)
traverseAMConcurrently = traverseAMImpl pooledMapConcurrently

-- | Contains the state used internally by 'wcc'.
data StateWCC a = WCC
  { component  :: {-# UNPACK #-} !Int  -- ^ A generator for labeling vertices based on their group.
  , components :: Map a Int  -- ^ A map from the vertices to their current label.
  , innerEdges :: IntMap (DList (a, a))  -- ^ Relates labels to their adjacency (difference) list.
  , mergedIn   :: IntMap Int  -- ^ Allows fast lookup of which labels have been joined with other label.
                              -- For instance, while performing the DFS, if we meet an older label,
                              -- we set that the current label was "marged" with the previous one.
  }

-- | Find all weakly connected components of a graph, that is, the disconnected subgraphs of a graph.
wcc :: forall a. Ord a => AdjacencyMap a -> [AdjacencyMap a]
wcc graph = joinGraphs $ flip execState (WCC minBound mempty mempty mempty) $
  for_ (G.vertexList graph) \v -> do
    stateWCC <- get
    unless (Map.member v (components stateWCC)) do
      let color = component stateWCC
      put stateWCC
        { component = 1 + color
        , components = Map.insert v color (components stateWCC)
        , innerEdges = IntMap.insert color [] (innerEdges stateWCC)
        }
      proccessDfsTree color v
  where
    proccessDfsTree :: Int -> a -> State (StateWCC a) ()
    proccessDfsTree color top =
      for_ (G.postSet top graph) \v -> do
        stateWCC <- get
        case Map.lookup v (components stateWCC) of
          Nothing -> do
            put stateWCC
              { components = Map.insert v color (components stateWCC)
              , innerEdges = IntMap.insertWith (<>) color [(top, v)] $ innerEdges stateWCC
              }
            proccessDfsTree color v
          Just newColor ->
            put stateWCC
              { innerEdges =
                  let
                    oldColor = IntMap.findWithDefault newColor newColor (mergedIn stateWCC)
                    (fromMaybe [] -> oldEdges, edges) =
                      IntMap.updateLookupWithKey (\_ _ -> Nothing) oldColor (innerEdges stateWCC)
                  in
                  IntMap.insertWith (<>) color (oldEdges `DList.snoc` (top, v)) edges
              , mergedIn = IntMap.insert newColor color (mergedIn stateWCC)
              }

    joinGraphs :: StateWCC a -> [AdjacencyMap a]
    joinGraphs WCC{components, innerEdges} =
      fmap
        (\(color, edges) -> bool (G.edges $ toList edges) (G.vertex (vertices IntMap.! color)) (null edges))
        (IntMap.toList innerEdges)
      where
        vertices :: IntMap a
        vertices = IntMap.fromList $ map swap $ Map.toList components
