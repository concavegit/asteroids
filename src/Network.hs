{-# LANGUAGE Arrows #-}

module Network
  ( network
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           Data.Either
import           FRP.Yampa
import           Network.Extra
import           SDL                 hiding (Event)
import           Types

network :: Game -> SF Controller Game
network game = proc ctrl -> do
  shipFlap <- edge -< ctrl ^. controllerFlap

  let gameShipRect = game ^. gameShip . shipSprite . spriteRect
  shipBottom <- edge -< gameShipRect ^. rectP . _y + gameShipRect ^. rectD . _y >= game ^. gameBounds . _y
  shipTop <- edge -< gameShipRect ^. rectP . _y <= 0

  shipCollideWrong <- edge -< or $ rectCollide (game ^. gameShip . shipSprite . spriteRect) . (^. asteroidSprite . spriteRect) <$> lefts (game ^. gameAsteroidBelt)
  shipCollideRight <- edge -< or $ rectCollide (game ^. gameShip . shipSprite . spriteRect) . (^. asteroidSprite . spriteRect) <$> rights (game ^. gameAsteroidBelt)

  asteroidsEnd <- edge -< (<= 0) . (^. asteroidSprite . spriteRect . rectP . _y) . either id id $ head (game ^. gameAsteroidBelt)

  score' <- accumHold (game ^. gameScore . score) -< (+1) <$ asteroidsEnd

  returnA -< execState
    ( gameQuit .= ctrl ^. controllerQuit
    >> gameScore . score .= score'
    ) game
