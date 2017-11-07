module Graph where

import Prelude
import Data.Int (toNumber)
import Data.List as L
import Data.List.NonEmpty as NL
import Data.Map as M
import Data.Set as S
import Data.Typelevel.Undefined (undefined)
import Data.Tuple as T
import Data.Foldable (class Foldable, foldMap, foldl)
import Data.Traversable (traverse, sequence)
import Data.Monoid (class Monoid)
import Data.Semigroup (class Semigroup)
import Data.Monoid (mempty)
import Data.Bifunctor (rmap)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (class Unfoldable)
import Control.Monad.Eff
import Control.Monad.Eff.Random (random, randomInt, randomRange, RANDOM)

import Random
 
newtype Graph = Graph (M.Map Int (S.Set Int))

instance semigroupGraph :: Semigroup Graph where
  append (Graph g1) (Graph g2) = Graph (M.unionWith (<>) g1 g2)

instance monoidGraph :: Monoid Graph where
  mempty = empty

instance showGraph :: Show Graph where
  show (Graph g) = show g
      
showGraphTree :: Graph -> String
showGraphTree (Graph g) = M.showTree g

showEdgeList :: Graph -> String
showEdgeList g = foldMap (\(T.Tuple u v) -> (show u) <> " " <> (show v) <> "\n") (edges g :: L.List (T.Tuple Int Int))

empty :: Graph
empty = Graph (M.empty)

singletonNode :: Int -> Graph
singletonNode u = Graph (M.singleton u S.empty)

singletonEdge :: Int -> Int -> Graph
singletonEdge u v = Graph (M.singleton u (S.singleton v)) <> singletonNode v

fromEdges :: forall f. Foldable f => Functor f => f (T.Tuple Int Int) -> Graph
fromEdges = foldMap (T.uncurry singletonEdge)

insertNode :: Int -> Graph -> Graph
insertNode u g = singletonNode u <> g

insertEdge :: Int -> Int -> Graph -> Graph
insertEdge u v g = singletonEdge u v <> g

neighbours :: Int -> Graph -> Maybe (S.Set Int)
neighbours i (Graph g) = M.lookup i g

flatEdges :: forall f. Unfoldable f => Functor f => (T.Tuple Int (S.Set Int)) -> f (T.Tuple Int Int)
flatEdges (T.Tuple u vs) = map (\v -> (T.Tuple u v)) (S.toUnfoldable vs)

nodes :: Graph -> L.List Int
nodes (Graph g) = M.keys g

edges :: forall f. Unfoldable f => Functor f => Bind f => Graph -> f (T.Tuple Int Int)
edges (Graph g) = join (map (\t -> flatEdges t) (M.toUnfoldable g))

undirected :: Graph -> Graph
undirected g = foldMap (\(T.Tuple u v) -> singletonEdge u v  <> singletonEdge v u) ((edges g) :: L.List (T.Tuple Int Int))

degree :: Int -> Graph -> Maybe Int
degree i g = map S.size (neighbours i g)

gBipole :: Graph
gBipole = undirected (singletonEdge 1 2)

gFork :: Graph
gFork = undirected $ fromEdges $ [T.Tuple 1 2,T.Tuple 1 3]

gErdosRenyi :: forall e. Number -> Int -> Eff (random :: RANDOM | e) Graph
gErdosRenyi p size = 
  do
    (mbEdges :: L.List (Maybe Graph)) <- (sequence <<< L.concat) $
      map (\u -> map (\v -> randomRange 0.0 1.0 >>= (\r ->
                              if r < p
                                then pure (Just (undirected $ singletonEdge u v))
                                else pure Nothing)) 
                     (L.range u size)) 
          (L.range 1 size)
    pure $ foldl (\g m -> g <> case m of
                                 Just e -> e
                                 Nothing -> empty)
                 empty
                 mbEdges

gWattsStrogatz :: forall e. Int -> Int -> Number -> Eff (random :: RANDOM | e) Graph
gWattsStrogatz n k beta =
  let 
    startingNodes = (L.range 1 n)
    regularRingEdges i = map (\i -> (mod ((mod (i - 1) n) + n) n) + 1) 
                             ((L.range (i - (k / 2)) (i - 1) ) <> (L.range (i + 1) (i + (k / 2))))
    startingEdges = foldMap (T.uncurry singletonEdge) $ 
                      L.concat (map (\n -> map (T.Tuple n) 
                                               (regularRingEdges n) ) 
                                    startingNodes)
    rewire :: forall e. (T.Tuple Int Int) -> Eff (random :: RANDOM | e) Graph 
    rewire (T.Tuple i j) = do
      yes <- random >>= (\r -> if r < beta then pure true else pure false)
      if (not yes)
        then
          pure (singletonEdge i j)
        else do
          newTarget <- map (\x -> if x < i then x else x + 1) (randomInt 1 (n - 1))
          pure $ singletonEdge i newTarget
  in do
    rewiredEdges <- sequence $ map rewire $ 
                      L.filter (\(T.Tuple i j) -> i < j) 
                               (edges startingEdges)
    pure $ undirected $ foldl (<>) empty rewiredEdges

gBarabasiAlbert :: forall e. Int -> Int -> Int -> Eff (random :: RANDOM | e) Graph
gBarabasiAlbert m0 m size = 
  let 
    startingNodes = foldMap singletonNode (L.range 1 m0)
    startingEdges :: Graph
    startingEdges = startingNodes <> 
      foldMap (T.uncurry singletonEdge) 
        (map (T.Tuple (m0 + 1)) (L.range 1 (min m m0)))
    newDestinations :: forall e1. Graph -> Eff (random :: RANDOM | e1) (L.List Int)
    newDestinations g = case NL.fromList (nodes g) of
      Nothing -> pure L.Nil
      Just ns -> do
        targets <- traverse (\_ -> weightedChoice 
          (map (\n -> T.Tuple n (case (degree n g) of 
                                   Nothing -> 0.0
                                   Just x -> toNumber x))
               ns))
          (L.range 1 m)
        pure (L.nub targets)
    newGraph :: forall e1. Graph -> Int -> Eff (random :: RANDOM | e1) Graph
    newGraph g n = do
      diff <- map fromEdges (map (map (T.Tuple n)) (newDestinations g))
      pure (g <> undirected diff)
    graphSteps :: forall e2. L.List (Graph -> Eff (random :: RANDOM | e2) Graph)
    graphSteps = map (flip newGraph) (L.range (m0 + 2) (size - m0 + 1))
  in do
    graph <- foldl (>>=) (pure startingEdges) graphSteps
    pure (undirected graph)
            
