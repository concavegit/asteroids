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
  mul <- randomRIO
    ( Mult 0 0 . replicate 4 $ Right 0
    , Mult 99 99 . replicate 4 $ Right 9801
    )

  f <- theFont
  let mulo = MultObj
        { _multObjPos = P $ V2 0 0
        , _multObjFont = f
        , _multObjMult = mul
        , _multObjColor = V4 0 0 255 255
        }

  sprite' <- getDataFileName "res/asteroid.bmp" >>= loadBMP
  let asts = genAsteroids (mul ^. multChoices) (world ^. worldDims . _y) Asteroid
        { _asteroidSprite = Sprite
          { _spriteRect = Rectangle (P $ V2 (world ^. worldDims . _x) 0) undefined
          , _sprite = sprite'}
        , _asteroidV = -20
        , _asteroidVMult = 1.05
        , _asteroidFont = f
        , _asteroidColor = V4 255 255 255 255
        }
  pure (mulo, asts)

ship0 :: IO Ship
ship0 =
  do
    sprite' <- getDataFileName "res/spaceship.bmp" >>= loadBMP
    pure Ship
      { _shipSprite = Sprite
        { _spriteRect = Rectangle (P $ V2 0 0) (V2 20 12)
        , _sprite = sprite'
        }
      , _shipV = 0
      , _shipVT = 40
      , _shipG = 80
      }

world :: World
world = World
  { _worldScale = 5
  , _worldDims = V2 128 96
  , _worldFPS = 60
  }

game0 :: IO Game
game0 = do
  ship <- ship0
  (mul, asters) <- multObjAster0
  f <- theFont
  (sx, sy) <- (both %~ (/ world ^. worldScale) . fromIntegral) <$> size f msg

  pure Game
    { _gameBounds = world ^. worldDims
    , _gameScore = Score
      { _scoreFont = f
      , _scoreColor = V4 255 255 255 255
      , _score = 0
      , _scorePoint = P $ V2 (world ^. worldDims . _x - 20) 0
      }
    , _gameMultRange =
      ( Mult {_multChoices = replicate 4 $ Right 0}
      , Mult {_multChoices = replicate 4 $ Right 9801}
      )
    , _gameShip = ship
    , _gameMultObj = mul
    , _gameAsteroidBelt = asters
    , _gameOver = False
    , _gameOverMsg = TextBox
      { _textBoxText = msg
      , _textBoxFont = f
      , _textBoxColor = V4 255 0 255 0
      , _textBoxPoint = P . V2 ((world ^. worldDims . _x - sx) / 2)
        $ (world ^. worldDims . _y - sy) / 2
      }
    , _gameQuit = False
    }

  where msg = "Game over, hit Space!"

main :: IO ()
main = game0 >>= asteroids world
