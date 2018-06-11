module Main where

import           Asteroids
import           Control.Lens
import           Paths_asteroids
import           SDL
import           Types

ship0 :: IO Ship
ship0 = Ship (Rectangle (P $ V2 0 0) (V2 20 12)) 0 40 80 <$> getDataFileName "res/spaceship.bmp"

world :: World
world = World 5 (V2 128 96) 120

game0 :: IO Game
game0 = do
  ship <- ship0
  pure Game
    { _gameBounds = (world ^. worldDims)
    , _gameShip = ship
    , _gameQuit = False}

main :: IO ()
main = game0 >>= asteroids world
