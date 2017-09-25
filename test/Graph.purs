module Test.Graph where

import Prelude
import Data.Typelevel.Undefined (undefined)

import Test.QuickCheck
import Test.QuickCheck.Gen

import Data.Maybe
import Data.List.Lazy
import Data.Array as A
import Data.Tuple
import Data.Lazy
import Data.Int
import Control.Applicative

-- import Math

import Graph

genRandomGraph :: Int -> Gen Graph
genRandomGraph n = map fromEdges edges
  where allEdges = map Tuple (A.range 1 n) <*> A.range 1 n
        edges = do
          nEdges <- chooseInt 0 (pow n 2)
          if nEdges == 0
            then pure []
            else map (A.take nEdges) (shuffle (A.fromFoldable allEdges))

-- select :: forall a. List Int -> List a -> List a
-- select indices elements = select' indices elements 0
--   where select' indices' elements' cur =
--           case (Tuple (head indices') (head elements')) of
--             (Tuple Nothing _) -> nil
--             (Tuple _ Nothing) -> nil
--             (Tuple (Just i) (Just e)) -> if i == cur
--               then (e : (select' (tailOrNil indices') (tailOrNil elements') (cur + 1)))
--               else select' indices' (tailOrNil elements') (cur + 1)
-- 
-- tailOrNil :: forall a. List a -> List a
-- tailOrNil l = case tail l of
--   Nothing -> nil
--   Just t -> t

propGraphAllEdgesDestinationExist :: Gen Result
propGraphAllEdgesDestinationExist = do
  g <- sized genRandomGraph
  let edgesg = ((edges g) :: List (Tuple Int Int))
  let dests = map snd edgesg
  let nodesg = nodes g
  pure $ all (\e -> elem e nodesg) dests 
         <?> ("Nodes : " <> show nodesg <> "\n" <>
              "Edges : " <> show edgesg)
  
  
