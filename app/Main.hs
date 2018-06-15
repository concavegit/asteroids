{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Asteroids
import           Control.Lens
import           Paths_asteroids
import           SDL
import           SDL.Font
import           System.Random
import           Types

theFont :: IO Font
theFont = SDL.Font.initialize >> getDataFileName "res/pro-font-windows.ttf" >>= flip load 40

multObjAster0 :: IO (MultObj, AsteroidBelt)
multObjAster0 = do
  mul <- randomRIO (Mult 0 0 . replicate 4 $ Left 0, Mult 99 99 . replicate 4 $ Left 9801)
  f <- theFont
  let mulo = MultObj
        { _multObjPos = P $ V2 0 0
        , _multObjFont = f
        , _multObjMult = mul
        , _multObjColor = V4 0 0 255 255
        }

  sprite <- getDataFileName "res/asteroid.bmp" >>= loadBMP
  let asts = genAsteroids (mul ^. multChoices) (world ^. worldDims . _y) Asteroid
        { _asteroidRect = Rectangle (P $ V2 0 0) undefined
        , _asteroidV = -20
        , _asteroidVMult = 1.1
        , _asteroidSprite = sprite
        , _asteroidFont = f
        , _asteroidColor = V4 255 255 255 255
        }
  pure (mulo, asts)

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
  , _worldFPS = 60}

game0 :: IO Game
game0 = do
  ship <- ship0
  (mul, asters) <- multObjAster0
  pure Game
    { _gameBounds = world ^. worldDims
    , _gameShip = ship
    , _gameMultObj = mul
    , _gameAsteroidBelt = asters
    , _gamePoints = 0
    , _gameOver = False
    , _gameQuit = False
    }

main :: IO ()
main = game0 >>= asteroids world
