module Main where

-- import Prelude
-- import Data.Int (toNumber)
-- import Data.Maybe (Maybe(..))
-- import Data.List as L
-- import Data.List.NonEmpty as NL
-- import Data.Traversable (sequence)
-- import Data.Time.Duration (Milliseconds(..))
-- import Control.Monad.Aff (Aff, launchAff_, delay)
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Class (liftEff)
-- import Control.Monad.Eff.Console (CONSOLE, log)
-- import Control.Monad.Eff.Random (RANDOM)
-- import Control.Monad.Eff.Timer (TIMER)
-- import Control.Monad.Loops (iterateUntilM)
-- import Graph as G
-- import Politoscope (gPolitoscope)
-- import Random as R

import React as R
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T

-- Thermite components define a type of states.
-- As a first example, we will create a component which uses a 
-- state of type Int:

type State = Int

-- A component needs an initial state:

initialState :: State
initialState = 42

-- The state is available to the render function:

render :: T.Render State _ _
render _ _ state _ =
  [ R.h1' [ R.text "Lesson 1 - State" ]
  , R.p' [ R.text "The state is: "
         , R.text (show state)
         ]
  , R.p'  [ R.text "Go to "
          , R.a [ RP.href "?gist=e1bdb15e580224e3b04398ddd28b6243&backend=thermite"
                , RP.target "_top"
                ]
                [ R.text "Lesson 2" ]
          , R.text "."
          ]
  ]

-- A more interesting component allows the user to modify its state.
-- We will see how Thermite supports this via "actions" in the next
-- lesson.

spec :: T.Spec _ State _ _
spec = T.simpleSpec T.defaultPerformAction render

main = T.defaultMain spec initialState

-- -- Returns Nothing if u isn't in g.
-- step :: forall e. G.Graph -> Int -> Eff (random :: RANDOM | e) (Maybe Int)
-- step g u = let mbNeighbours = G.neighbours u g
--            in case mbNeighbours of
--              Just neighbours ->
--                case NL.fromList $ L.fromFoldable neighbours of
--                  Just neNeighbours ->
--                    let mbDegrees = sequence $ map ((flip G.degree) g) neNeighbours
--                    in case mbDegrees of
--                      Just degrees -> map Just $ R.weightedChoice (NL.zip neNeighbours (map toNumber degrees))
--                      Nothing -> pure (Just u)
--                  Nothing -> pure (Just u)
--              Nothing -> pure Nothing
-- 
-- wait :: forall e. G.Graph -> Int -> Aff (console :: CONSOLE, random :: RANDOM, timer :: TIMER | e) Unit
-- wait g u = case (G.degree u g) of
--   Nothing -> pure unit
--   Just 0 -> pure unit
--   Just d -> do
--     delay (Milliseconds (1000.0 / (toNumber d)))
--     pure unit
-- 
-- go :: forall e. G.Graph -> Int -> Aff (console :: CONSOLE, random :: RANDOM, timer :: TIMER| e) (Maybe Int)
-- go g u = iterateUntilM
--   (\mbV -> case mbV of
--     Just v -> false
--     Nothing -> true)
--   (\mbU -> case mbU of
--     Just u -> do
--       wait g u
--       v <- liftEff $ step g u
--       liftEff $ log (show v)
--       liftEff $ tick
--       pure v
--     Nothing -> do
--       liftEff $ log "Can't Jump."
--       pure Nothing)
--   (Just u)
-- 
-- foreign import tick :: forall e. Eff (console :: CONSOLE | e) Unit
-- 
-- graphErdosRenyi :: forall e. Eff (console :: CONSOLE, random :: RANDOM, timer :: TIMER| e) Unit
-- graphErdosRenyi = launchAff_ do
--   g <- liftEff $ G.gErdosRenyi 0.1 60
--   _ <- liftEff $ log $ G.showGraphTree g
--   _ <- liftEff $ log $ G.showEdgeList g
--   _ <- go g 1
--   pure unit
-- 
-- graphBarabasiAlbert :: forall e. Eff (console :: CONSOLE, random :: RANDOM, timer :: TIMER| e) Unit
-- graphBarabasiAlbert = launchAff_ do
--   g <- liftEff $ G.gBarabasiAlbert 3 3 60
--   _ <- liftEff $ log $ G.showGraphTree g
--   _ <- liftEff $ log $ G.showEdgeList g
--   _ <- go g 1
--   pure unit
-- 
-- graphWattsStrogatz :: forall e. Eff (console :: CONSOLE, random :: RANDOM, timer :: TIMER| e) Unit
-- graphWattsStrogatz = launchAff_ do
--   g <- liftEff $ G.gWattsStrogatz 60 6 0.6 
--   _ <- liftEff $ log $ G.showGraphTree g
--   _ <- liftEff $ log $ G.showEdgeList g
--   _ <- go g 1
--   pure unit
-- 
-- graphPolitoscope :: forall e. Eff (console :: CONSOLE, random :: RANDOM, timer :: TIMER| e) Unit
-- graphPolitoscope = launchAff_ do
--   _ <- go gPolitoscope 1
--   pure unit
-- 
-- showExamples :: forall e. Eff (console :: CONSOLE, random :: RANDOM, timer :: TIMER| e) Unit
-- showExamples = launchAff_ do
--   _ <- liftEff $ log $ "ErdosRenyi 0.1 20"
--   ger <- liftEff $ G.gErdosRenyi 0.1 20
--   _ <- liftEff $ log $ G.showEdgeList ger
--   _ <- liftEff $ log $ "WattsStrogatz 20 4 0.6"
--   gws <- liftEff $ G.gWattsStrogatz 20 4 0.2 
--   _ <- liftEff $ log $ G.showEdgeList gws
--   _ <- liftEff $ log $ "BarabasiAlbert 1 1 20"
--   gba <- liftEff $ G.gBarabasiAlbert 1 1 20 
--   _ <- liftEff $ log $ G.showEdgeList gba
--   pure unit
-- 
-- 
-- main :: forall e. Eff (console :: CONSOLE, random :: RANDOM, timer :: TIMER| e) Unit
-- -- main = pure unit
-- main = showExamples
