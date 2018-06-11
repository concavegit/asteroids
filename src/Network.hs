{-# LANGUAGE Arrows #-}

module Network
  ( network
  ) where

import           Behavior
import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           FRP.Yampa
import           Types

network :: Game -> SF Controller Game
network game = proc ctrl -> do
  ship <- shipFall $ game ^. gameShip -< game ^. gameShip
  returnA -< game
    & execState (gameShip .= ship >> gameQuit .= ctrl ^. controllerQuit)
