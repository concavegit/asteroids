{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Asteroids
import           Control.Lens
import           Paths_asteroids
import           SDL
import           SDL.Font
import           System.Random
import           Types

mult0 :: IO Mult
mult0 = randomRIO (Mult 0 0 . replicate 4 $ Left 0, Mult 99 99 . replicate 4 $ Left 9801)

asteroids0 :: IO [Either Asteroid Asteroid]
asteroids0 = do
  SDL.Font.initialize
  font <- getDataFileName "res/pro-font-windows.ttf" >>= flip load 40
  sprite <- getDataFileName "res/asteroid.bmp" >>= loadBMP
  mul <- mult0
  pure $ genAsteroids (mul ^. multChoices) 0 (world ^. worldDims . _y) 0 sprite font (V4 255 255 255 255)

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
  mul <- mult0
  asters <- asteroids0
  pure Game
    { _gameBounds = world ^. worldDims
    , _gameShip = ship
    , _gameMult = mul
    , _gameAsteroids = asters
    , _gameQuit = False
    }

main :: IO ()
main = game0 >>= asteroids world
