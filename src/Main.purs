module Main where

import Prelude
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.List as L
import Data.List.NonEmpty as NL
import Data.Traversable (sequence)
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Aff (Aff, launchAff_, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Loops (iterateUntilM)
import Graph as G 
import Random as R


-- Returns Nothing if u isn't in g.
step :: forall e. G.Graph -> Int -> Eff (random :: RANDOM | e) (Maybe Int)
step g u = let mbNeighbours = G.neighbours u g
           in case mbNeighbours of
             Just neighbours -> 
               case NL.fromList $ L.fromFoldable neighbours of 
                 Just neNeighbours -> 
                   let mbDegrees = sequence $ map ((flip G.degree) g) neNeighbours
                   in case mbDegrees of 
                     Just degrees -> map Just $ R.weightedChoice (NL.zip neNeighbours (map toNumber degrees))
                     Nothing -> pure (Just u)
                 Nothing -> pure (Just u)
             Nothing -> pure Nothing

wait :: forall e. G.Graph -> Int -> Aff (console :: CONSOLE, random :: RANDOM, timer :: TIMER | e) Unit
wait g u = case (G.degree u g) of
  Nothing -> pure unit
  Just 0 -> pure unit
  Just d -> do 
    delay (Milliseconds (1000.0 / (toNumber d)))
    pure unit

go :: forall e. G.Graph -> Int -> Aff (console :: CONSOLE, random :: RANDOM, timer :: TIMER| e) (Maybe Int)
go g u = iterateUntilM
  (\mbV -> case mbV of
    Just v -> false 
    Nothing -> true)
  (\mbU -> case mbU of 
    Just u -> do
      wait g u 
      v <- liftEff $ step g u
      liftEff $ log (show v)
      liftEff $ tick
      pure v
    Nothing -> do
      liftEff $ log "Can't Jump."
      pure Nothing)
  (Just u)

foreign import tick :: forall e. Eff (console :: CONSOLE | e) Unit

graphRandom :: forall e. Eff (console :: CONSOLE, random :: RANDOM, timer :: TIMER| e) Unit
graphRandom = launchAff_ do
  -- let g = G.gBipole
  -- let g = G.gFork
  g <- liftEff $ G.gRandom 0.1 50
  _ <- liftEff $ log $ show g
  _ <- go g 1
  pure unit

graphPA :: forall e. Eff (console :: CONSOLE, random :: RANDOM, timer :: TIMER| e) Unit
graphPA = launchAff_ do
  -- let g = G.gBipole
  -- let g = G.gFork
  g <- liftEff $ G.gPreferentialAttachment 3 3 50
  _ <- liftEff $ log $ show g
  _ <- go g 1
  pure unit

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM, timer :: TIMER| e) Unit
main = pure unit
