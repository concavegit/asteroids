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
    e <- asteroidBeltAroundE . (^. asteroidSprite . spriteRect . rectP . _x)
      . either id id . head $ game ^. gameAsteroidBelt
      -< game & gameAsteroidBelt .~ asters
    mult' <- hold (game ^. gameMultObj . multObjMult) -< multGen <$ e
    x <- hold False -< True <$ e

    score' <- accumHold 0 -< (+1) <$ e

  returnA -< execState
    ( gameAsteroidBelt .= asters
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
  x <- integral >>> (<= 0) . (+ d) ^>> edge -< view asteroidV . either id id . head $ g ^. gameAsteroidBelt
  returnA -< dup $ g <$ x

asteroidBeltAroundE :: Double -> SF Game (Event Game)
asteroidBeltAroundE d = dSwitch (gameBoundTraversed d) $ asteroidBeltAroundE . (^. gameBounds . _x)
