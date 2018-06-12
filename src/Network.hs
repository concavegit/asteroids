{-# LANGUAGE Arrows #-}

module Network
  ( network
  ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           FRP.Yampa
import           SDL                 hiding (Event)
import           Types

network :: Game -> SF Controller Game
network game = proc ctrl -> do
  fall <- shipFlapFallBounded game -< ctrl
  returnA -< gameQuitF (game & gameShip .~ fall) ctrl

gameQuitF :: Game -> Controller -> Game
gameQuitF game = ($ game) . set gameQuit . view controllerQuit

shipFall :: Ship -> SF a Ship
shipFall ship = proc _ -> do
  rec
    let a = g - v * g / vT
    v <- (+ ship ^. shipV) ^<< integral -< a
    y <- integral >>^ (+ ship ^. shipRect . rectP . _y) -< v
  returnA -< execState (shipV .= y >> shipRect . rectP . _y .= y) ship

  where g = ship ^. shipG
        vT = ship ^. shipVT

shipFlap' :: (a -> SF Controller b) -> a -> SF Controller (b, Event b)
shipFlap' f ship0 = proc ctrl -> do
  ship <- f ship0 -< ctrl
  control <- edge -< ctrl ^. controllerFlap
  returnA -< (ship, ship <$ control)

shipFlap :: (Ship -> SF Controller Ship) -> Ship -> SF Controller Ship
shipFlap f ship0 = switch (shipFlap' f ship0)
  $ \ship -> shipFlap f (ship & shipV .~ -ship ^. shipVT)

stopShip' :: Double -> Ordering -> (a -> SF b Ship) -> a
  -> SF b (Ship, Event Ship)
stopShip' bound comp f ship0 = proc ship -> do
  ship' <- f ship0 -< ship
  hit <- edge -< compare (ship' ^. shipRect . rectP . _y) bound == comp
  returnA -< (ship', ship' <$ hit)

stopShip :: Double -> Ordering -> (Ship -> SF a Ship) -> Ship -> SF a Ship
stopShip bound comp f ship0 = switch (stopShip' bound comp f ship0)
  $ \ship -> stopShip bound comp f
  (execState
   ( shipRect . rectP . _y .= bound
   >> shipV .= 0
   ) ship)

shipBottom :: (Game -> SF a Ship) -> Game -> SF a Ship
shipBottom f game = stopShip
  (game ^. gameBounds . _y - game ^. gameShip . shipRect . rectD . _y)
  GT (f . ($ game) . set gameShip) (game ^. gameShip)

shipTop :: (Game -> SF a Ship) -> Game -> SF a Ship
shipTop f game = stopShip 0 LT (f . ($ game) . set gameShip)
  (game ^. gameShip)

shipInBounds :: (Game -> SF a Ship) -> Game -> SF a Ship
shipInBounds f = shipTop $ shipBottom f

shipFallBounded :: Game -> SF a Ship
shipFallBounded = shipInBounds $ shipFall . view gameShip

shipFlapFallBounded :: Game -> SF Controller Ship
shipFlapFallBounded game = shipFlap
  (shipFallBounded . ($ game) . set gameShip) $ game ^. gameShip
