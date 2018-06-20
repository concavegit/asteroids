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
  -- t1 <- shipAsteroid game -< (ctrl, ())
  g <- sas game -< (ctrl, ())
  -- (fall, asters) <- shipCollide  -< t1
  returnA -< gameQuitF ctrl g

shipFall :: Ship -> SF a Ship
shipFall ship = proc _ -> do
  rec
    let a = g - v * g / vT
    v <- integral >>^ (+ ship ^. shipV) -< a
    y <- integral >>^ (+ ship ^. shipRect . rectP . _y) -< v
  returnA -< execState (shipV .= y >> shipRect . rectP . _y .= y) ship

  where
    g = ship ^. shipG
    vT = ship ^. shipVT

shipFlap' :: (a -> SF Controller b) -> a -> SF Controller (b, Event b)
shipFlap' f ship0 = f ship0 &&& (view controllerFlap ^>> edge)
  >>^ \(a, b) -> (a, a <$ b)

shipFlap :: (Ship -> SF Controller Ship) -> Ship -> SF Controller Ship
shipFlap = switch1 shipFlap' $ (&) <*> set shipV . negate . view shipVT

stopShip' :: Double -> Ordering -> (a -> SF b Ship) -> a
  -> SF b (Ship, Event Ship)
stopShip' bound comp
  = ((>>> edgeCond
      ((== comp) . flip compare bound . view (shipRect . rectP . _y))
     ) .)

stopShip :: Double -> Ordering -> (Ship -> SF a Ship) -> Ship -> SF a Ship
stopShip bound comp = switch1 (stopShip' bound comp)
  $ execState ( shipRect . rectP . _y .= bound >> shipV .= 0)

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

asteroidsForward :: AsteroidBelt -> SF a AsteroidBelt
asteroidsForward belt = proc _ -> integral
  >>^ flip eitherFmap belt . (asteroidRect . rectP . _x .~)
  . (+ aster ^. asteroidRect . rectP . _x) -< aster ^. asteroidV
  where aster = either id id $ head belt

asteroidsEnd :: (a -> SF b AsteroidBelt) -> a
  -> SF b (AsteroidBelt, Event AsteroidBelt)
asteroidsEnd
  = ((>>> edgeCond
      ((((< 0) .) . (+) <$> view (rectP . _x) <*> view (rectD . _x))
       . view asteroidRect . either id id . head)) .)

asteroidsLoop :: (Game -> SF a AsteroidBelt) -> Game -> SF a AsteroidBelt
asteroidsLoop f game0 = switch1 asteroidsEnd
  (\asters -> game0
    & gameAsteroidBelt .~ eitherFmap
    (execState
     ( asteroidRect . rectP . _x .= game0 ^. gameBounds . _x
       >> asteroidV *= either id id (head asters) ^. asteroidVMult
     )) asters
  ) f game0

incPoints f game0 = proc a -> do
  (_, asterE) <- asteroidsEnd f game0 -< a
  asters <- asteroidsLoop f game0 -< a
  pts <- accumHold 0 -< (+ 1) <$ asterE
  returnA -< execState (gameAsteroidBelt .= asters >> gamePoints .= pts) game0

asteroidsForwardLoop :: Game -> SF a AsteroidBelt
asteroidsForwardLoop = asteroidsLoop $ asteroidsForward . view gameAsteroidBelt

shipAsteroid :: Game -> SF (Controller, a) Game
shipAsteroid game0 = proc (ctrl, a) -> do
   ship <- shipFlapFallBounded game0 -< ctrl
   asteroidGame <- incPoints (asteroidsForward . view gameAsteroidBelt) game0  -< a
   returnA -< (gameShip .~ ship) asteroidGame

shipCollideWrong :: SF (Ship, [Either Asteroid b]) (Event ())
shipCollideWrong = or . uncurry fmap
  . (rectCollide . view shipRect *** map (view asteroidRect) . lefts) ^>> edge

-- shipAsteroidStop' :: (t -> SF (a, b1) (Ship, [Either Asteroid b2])) -> t
--   -> SF (a, b1)
  -- ((Ship, [Either Asteroid b2]), Event (Ship, [Either Asteroid b2]))
shipAsteroidStop' f g0 = proc c@(ctrl, a) -> do
  g <- f g0 -< (ctrl, a)
  e <- shipCollideWrong -< (g ^. gameShip, g ^. gameAsteroidBelt)
  returnA -< (g, g <$ e)

-- shipAsteroidStop :: (Game -> SF (a, b) (Ship, [Either Asteroid Asteroid]))
--   -> Game -> SF (a, b) (Ship, [Either Asteroid Asteroid])
shipAsteroidStop f g0 = switch (shipAsteroidStop' f g0) $ \g ->
  shipAsteroidStop f $ (execState (gameShip . shipV .= 0 >> gameShip . shipG .= 0 >> gameAsteroidBelt .= eitherFmap (asteroidV .~ 0) (g ^. gameAsteroidBelt))) g
  -- . (\(s, a) -> execState (gameShip .= s >> gameAsteroidBelt .= a) g0)
  -- . (execState (shipV .= 0 >> shipG .= 0 >> shipVT .= 0)
     -- *** eitherFmap (asteroidV .~ 0))

-- sas :: Game -> SF (Controller, b) (Ship, [Either Asteroid Asteroid])
sas g0 = shipAsteroidStop shipAsteroid g0

-- shipAsteroidStop = switch

  -- proc (s, a) -> do
  -- ship <- constant >>> (switch (shipCollideWrong)) -< s
  -- returnA -< a

-- -- shipCollide :: SF (Ship, AsteroidBelt) (Ship, AsteroidBelt)
-- -- shipCollide = switch shipCollideWrong constant
