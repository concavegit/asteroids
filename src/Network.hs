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
  asters <- asteroidsForwardAround game -< game
  returnA -< gameQuitF
    (execState (gameShip .= fall >> gameAsteroidBelt .= asters) game) ctrl

gameQuitF :: Game -> Controller -> Game
gameQuitF game = ($ game) . set gameQuit . view controllerQuit

shipFall :: Ship -> SF a Ship
shipFall ship = proc _ -> do
  rec
    let a = g - v * g / vT
    v <- (+ ship ^. shipV) ^<< integral -< a
    y <- integral >>^ (+ ship ^. shipRect . rectP . _y) -< v
  returnA -< execState (shipV .= y >> shipRect . rectP . _y .= y) ship

  where
    g = ship ^. shipG
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

asteroidBeltMap :: Functor f => (a -> b) -> f (Either a a) -> f (Either b b)
asteroidBeltMap f = fmap $ either (Left . f) (Right . f)

asteroidsForward :: AsteroidBelt -> SF a AsteroidBelt
asteroidsForward belt = proc _ -> integral
  >>^ flip asteroidBeltMap belt . (asteroidRect . rectP . _x .~)
  . (+ aster ^. asteroidRect . rectP . _x) -< aster ^. asteroidV
  where aster = either id id $ head belt

asteroidsBackAround' :: (a -> SF b AsteroidBelt) -> a -> SF b (AsteroidBelt, Event AsteroidBelt)
asteroidsBackAround' f game0 = proc game -> do
  asters <- f game0 -< game
  let rect = view asteroidRect . either id id $ head asters
  past <- edge -< rect ^. rectP . _x + rect ^. rectD . _x < 0
  returnA -< (asters, asters <$ past)

asteroidsBackAround :: (Game -> SF a AsteroidBelt) -> Game -> SF a AsteroidBelt
asteroidsBackAround f game0 = switch (asteroidsBackAround' f game0)
  $ \asters -> asteroidsBackAround f $ game0
  & gameAsteroidBelt .~ asteroidBeltMap
  (asteroidRect . rectP . _x .~ game0 ^. gameBounds . _x) asters

asteroidsForwardAround :: Game -> SF Game AsteroidBelt
asteroidsForwardAround = asteroidsBackAround $ asteroidsForward . view gameAsteroidBelt
