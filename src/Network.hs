{-# LANGUAGE Arrows #-}

module Network
  ( network
  ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           FRP.Yampa
import           Network.Extra
import           SDL                 hiding (Event)
import           Types

network :: RandomGen g => g -> Game -> SF Controller Game
network gen game = proc ctrl -> do
  rec
    multGen <- noiseR (game ^. gameMultRange) gen -< ()

    let generate = genAsteroids (mult' ^. multChoices) (game ^. gameBounds . _y) . either id id  $ head forward
        vMult = (view asteroidVMult . either id id . head) generate <$ astersEnd
    -- accel <- accumHold (game ^. gameAsteroidBelt) -< eitherFmap (asteroidV *~ (view asteroidVMult . either id id . head $ game ^. gameAsteroidBelt)) <$ astersEnd
    accel <- astersAccel (view asteroidV . either id id . head $ game ^. gameAsteroidBelt) -< (generate, vMult)

    forward <- asteroidBeltForward (game ^. gameAsteroidBelt) >>> iPre (game ^. gameAsteroidBelt) -< accel
    offset <- accumHold 0 -< (+ ((^. asteroidSprite . spriteRect . rectD . _x) . either id id . head) forward) . (+ game ^. gameBounds . _x) <$ astersEnd

    let looped = eitherFmap (asteroidSprite . spriteRect . rectP . _x +~ offset) forward
    astersEnd <- asteroidBeltAroundE . (^. asteroidSprite . spriteRect . rectP . _x)
      . either id id . head $ game ^. gameAsteroidBelt
      -< game & gameAsteroidBelt .~ forward

    mult' <- hold (game ^. gameMultObj . multObjMult) -< multGen <$ astersEnd

    score' <- accumHold 0 -< (+1) <$ astersEnd

  returnA -< execState
    ( gameAsteroidBelt .= looped
    >> gameMultObj . multObjMult .= mult'
    >> gameQuit .= ctrl ^. controllerQuit
    >> gameScore . score .= score'
    ) game

asteroidBeltForward :: AsteroidBelt -> SF AsteroidBelt AsteroidBelt
asteroidBeltForward belt0 = proc belt -> do
  move <- integral >>^ (asteroidSprite . spriteRect . rectP . _x .~)
    . (+ either id id (head belt0) ^. asteroidSprite . spriteRect . rectP . _x)
    -< either id id (head belt) ^. asteroidV
  returnA -< eitherFmap move belt

gameBoundTraversed :: Double -> SF Game (Event Game, Event Game)
gameBoundTraversed d = proc g -> do
  y <- integral >>^ (+ d) -< view asteroidV . either id id . head $ g ^. gameAsteroidBelt
  x <- edge -< (<= 0) . (+ y) . (^. asteroidSprite . spriteRect . rectD . _x) . either id id . head $ g ^. gameAsteroidBelt
  returnA -< dup $ g <$ x

asteroidBeltAroundE :: Double -> SF Game (Event Game)
asteroidBeltAroundE d = dSwitch (gameBoundTraversed d) $ asteroidBeltAroundE . (^. gameBounds . _x)

astersAccel :: Double -> SF (AsteroidBelt, Event Double) AsteroidBelt
astersAccel d = proc (a, e) -> do
  v <- accumHold d -< (* (view asteroidVMult . either id id $ head a)) <$ e
  returnA -< eitherFmap (asteroidV .~ v) a
