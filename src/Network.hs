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

    let generate = genAsteroids (mult' ^. multChoices) (game ^. gameBounds . _y)
          $ forward ^. asteroidBeltHead
        vMult = generate ^. asteroidBeltHead . asteroidVMult <$ astersEnd

    accel <- astersAccel
      $ game ^. gameAsteroidBelt . asteroidBeltHead . asteroidV
      -< (generate, vMult)

    forward <- astersForward (game ^. gameAsteroidBelt)
      >>> iPre (game ^. gameAsteroidBelt) -< accel

    offset <- accumHold 0
      -< ( + forward
         ^. asteroidBeltHead . asteroidSprite . spriteRect . rectD . _x
        ) . (+ game ^. gameBounds . _x) <$ astersEnd

    let looped = eitherFmap (asteroidSprite . spriteRect . rectP . _x +~ offset)
          forward

    astersEnd <- astersAroundE
      ( game ^. gameAsteroidBelt . asteroidBeltHead . asteroidSprite
        . spriteRect . rectP . _x
      ) -< game & gameAsteroidBelt .~ forward

    mult' <- hold (game ^. gameMultObj . multObjMult) -< multGen <$ astersEnd

    score' <- accumHold 0 -< (+1) <$ astersEnd

  returnA -< execState
    ( gameAsteroidBelt .= looped
    >> gameMultObj . multObjMult .= mult'
    >> gameQuit .= ctrl ^. controllerQuit
    >> gameScore . score .= score'
    ) game

astersForward :: AsteroidBelt -> SF AsteroidBelt AsteroidBelt
astersForward belt0 = proc belt -> do
  move <- integral >>^ (asteroidSprite . spriteRect . rectP . _x .~)
    . (+ belt0 ^. asteroidBeltHead . asteroidSprite . spriteRect . rectP . _x)
    -< belt ^. asteroidBeltHead . asteroidV
  returnA -< eitherFmap move belt

gameBoundTraversed :: Double -> SF Game (Event Game, Event Game)
gameBoundTraversed d = proc g -> do
  x <- integral >>^ (+ d)
    -< (g ^. gameAsteroidBelt . asteroidBeltHead . asteroidV)
  e <- edge -< x
    + ( g ^. gameAsteroidBelt . asteroidBeltHead . asteroidSprite . spriteRect
       . rectD . _x
      ) <= 0
  returnA -< dup $ g <$ e

astersAroundE :: Double -> SF Game (Event Game)
astersAroundE d = dSwitch (gameBoundTraversed d)
  $ astersAroundE . (^. gameBounds . _x)

astersAccel :: Double -> SF (AsteroidBelt, Event Double) AsteroidBelt
astersAccel d = proc (a, e) -> do
  v <- accumHold d -< (* a ^. asteroidBeltHead . asteroidVMult) <$ e
  returnA -< eitherFmap (asteroidV .~ v) a
