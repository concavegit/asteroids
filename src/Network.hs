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

    asteroidBeltEnd <- edge -< (<= 0)
      . ((+) <$> (^. asteroidSprite . spriteRect . rectP . _x)
         <*> (^. asteroidSprite . spriteRect . rectD . _x))
      . either id id . head
      $ game ^. gameAsteroidBelt

    mult' <- hold (game ^. gameMultObj . multObjMult) -< multGen <$ asteroidBeltEnd
    let asteroidBelt' = genAsteroids (mult' ^. multChoices) (game ^. gameBounds . _y) . either id id  $ head asters
    asters <- asteroidBeltForward (game ^. gameAsteroidBelt) >>> iPre (game ^. gameAsteroidBelt) -< asteroidBelt'

    score <- accum 0 -< (+1) <$ asteroidBeltEnd

  returnA -< execState
    ( gameAsteroidBelt .= asters
    >> gameQuit .= ctrl ^. controllerQuit
    ) game

-- asteroidBeltForward :: AsteroidBelt -> SF a AsteroidBelt
-- asteroidBeltForward = (aster ^. asteroidV) >>> integral
--   >>^ flip eitherFmap belt . (asteroidSprite . spriteRect . rectP . _x .~)
--   . (+ aster ^. asteroidSprite . spriteRect . rectP . _x)
--   where aster = either id id $ head belt

asteroidBeltForward :: AsteroidBelt -> SF AsteroidBelt AsteroidBelt
asteroidBeltForward belt0 = proc belt -> do
  move <- integral >>^ (asteroidSprite . spriteRect . rectP . _x .~)
    . (+ either id id (head belt0) ^. asteroidSprite . spriteRect . rectP . _x)
    -< either id id (head belt) ^. asteroidV
  returnA -< eitherFmap move belt

-- asteroidBeltEdge :: Game -> SF Game (Event Game, Event Game)
-- asteroidBeltEdge g0 = proc g -> do
--   e <- edge -< (<= 0) . ((+) <$> (^. rectP . _x) <*> (^. rectD . _x)) . (^. asteroidSprite . spriteRect) . either id id . head $ g ^. gameAsteroidBelt
--   returnA -< dup $ g <$ e
