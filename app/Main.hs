module Main where

import           Asteroids
import           Control.Lens
import           Paths_asteroids
import           SDL
import           Types

ship0 :: IO Ship
ship0 =
  do
    sprite <- getDataFileName "res/spaceship.bmp" >>= loadBMP
    pure Ship
      { _shipRect = Rectangle (P $ V2 0 0) (V2 20 12)
      , _shipV = 0
      , _shipVT = 40
      , _shipG = 80
      , _shipSprite = sprite
      }

world :: World
world = World
  { _worldScale = 5
  , _worldDims = V2 128 96
  , _worldFPS = 120}

game0 :: IO Game
game0 = do
  ship <- ship0
  pure Game
    { _gameBounds = world ^. worldDims
    , _gameShip = ship
    , _gameQuit = False
    }

main :: IO ()
main = game0 >>= asteroids world
