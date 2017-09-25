module Random where

import Prelude
import Data.List.NonEmpty
import Data.Typelevel.Undefined (undefined)
import Data.Tuple
import Data.Foldable (class Foldable, foldl)
import Data.Monoid (class Monoid)
import Data.Semigroup (class Semigroup)
import Data.Bifunctor (rmap)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (class Unfoldable)
import Control.Monad.Eff
import Control.Monad.Eff.Random (RANDOM, randomRange)

weightedChoice :: forall a e. NonEmptyList (Tuple a Number) -> Eff (random :: RANDOM | e ) a
weightedChoice choices =
  let sum = foldl add 0.0 (map snd choices)
  in do 
    r <- randomRange 0.0 sum
    pure (weightedGet choices r) 

weightedGet :: forall a. NonEmptyList (Tuple a Number) -> Number -> a
weightedGet l x = if x < curWeight 
                    then curElem
                    else case fromList (tail l) of
                      (Just tl) -> weightedGet (tl) (x - curWeight)
                      Nothing -> curElem
  where (Tuple curElem curWeight) = head l
       
