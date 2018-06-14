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
shipFlap = switchAux shipFlap' $ (&) <*> set shipV . negate . view shipVT

switchAux :: (t -> b1 -> SF a (b2, Event c)) -> (c -> b1) -> t -> b1 -> SF a b2
switchAux g h f = flip switch (switchAux g h f . h) . g f

stopShip' :: Double -> Ordering -> (a -> SF b Ship) -> a
  -> SF b (Ship, Event Ship)
stopShip' bound comp
  = ((>>> edgeTagCond
      ((== comp) . flip compare bound . view (shipRect . rectP . _y))
     ) .)

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

eitherFmap :: Functor f => (a -> b) -> f (Either a a) -> f (Either b b)
eitherFmap = fmap . liftA2 either (Left .) (Right .)

asteroidsForward :: AsteroidBelt -> SF a AsteroidBelt
asteroidsForward belt = proc _ -> integral
  >>^ flip eitherFmap belt . (asteroidRect . rectP . _x .~)
  . (+ aster ^. asteroidRect . rectP . _x) -< aster ^. asteroidV
  where aster = either id id $ head belt

asteroidsEnd :: (a -> SF b AsteroidBelt) -> a
  -> SF b (AsteroidBelt, Event AsteroidBelt)
asteroidsEnd
  = ((>>> edgeTagCond
      ((((< 0) .) . (+) <$> view (rectP . _x) <*> view (rectD . _x))
       . view asteroidRect . either id id . head)) .)

asteroidsBackAround :: (Game -> SF a AsteroidBelt) -> Game -> SF a AsteroidBelt
asteroidsBackAround f game0 = switch (asteroidsEnd f game0)
  $ \asters -> asteroidsBackAround f $ game0
  & gameAsteroidBelt .~ eitherFmap
  (execState
   ( asteroidRect . rectP . _x .= game0 ^. gameBounds . _x
   >> asteroidV *= either id id (head asters) ^. asteroidVMult
   )
  ) asters

asteroidsForwardAround :: Game -> SF Game AsteroidBelt
asteroidsForwardAround = asteroidsBackAround
  $ asteroidsForward . view gameAsteroidBelt

rectCollide :: (Num t, Ord t) => Rectangle t -> Rectangle t -> Bool
Rectangle (P p1) d1 `rectCollide` Rectangle (P p2) d2 = within p2 (p1 + d1)
  && within (p1 - d2) p2
  where within = (and .) . liftA2 (<=)

shipAsteroid :: Game -> SF (Controller, Game) (Ship, AsteroidBelt)
shipAsteroid = liftA2 (***) shipFlapFallBounded asteroidsForwardAround

edgeTagCond :: (a -> Bool) -> SF a (a, Event a)
edgeTagCond f = dup ^>> second (dup ^>> second (f ^>> edge) >>^ uncurry (<$))

shipCollideWrong :: SF (Ship, AsteroidBelt)
  ((Ship, AsteroidBelt), Event (Ship, AsteroidBelt))
shipCollideWrong = edgeTagCond
  $ or . uncurry fmap
  . (rectCollide . view shipRect *** map (view asteroidRect) . lefts)
