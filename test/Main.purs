module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Test.Graph
import Test.QuickCheck

main :: forall e. Eff (exception :: EXCEPTION, random :: RANDOM, console :: CONSOLE | e) Unit
main = do
  quickCheck propGraphAllEdgesDestinationExist
