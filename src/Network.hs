{-# LANGUAGE Arrows #-}

module Network
  ( network
  ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           Data.Either
import           FRP.Yampa
import           Network.Extra
import           SDL                 hiding (Event)
import           Types

network :: RandomGen g => g -> Game -> SF Controller Game
network gen game = proc ctrl -> do
  rec
    multGen <- noiseR (game ^. gameMultRange) gen -< ()

    let asteroidBelt' = genAsteroids (mult' ^. multChoices) (game ^. gameBounds . _y) . either id id  $ head asters
    asters <- asteroidBeltForward (game ^. gameAsteroidBelt) >>> iPre (game ^. gameAsteroidBelt) -< asteroidBelt'
    astersX <- integral -< view asteroidV . either id id $ head asters
    offset <- accumHold 0 -< (+ ((^. asteroidSprite . spriteRect . rectD . _x) . either id id . head) asters) . (+ game ^. gameBounds . _x) <$ astersEnd
    let asters' = eitherFmap (asteroidSprite . spriteRect . rectP . _x +~ offset) asters
    astersEnd <- asteroidBeltAroundE . (^. asteroidSprite . spriteRect . rectP . _x)
      . either id id . head $ game ^. gameAsteroidBelt
      -< game & gameAsteroidBelt .~ asters
    mult' <- hold (game ^. gameMultObj . multObjMult) -< multGen <$ astersEnd
    x <- hold False -< True <$ astersEnd

    score' <- accumHold 0 -< (+1) <$ astersEnd

  returnA -< execState
    ( gameAsteroidBelt .= asters'
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
