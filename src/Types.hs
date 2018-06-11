{-# LANGUAGE TemplateHaskell #-}

module Types
  ( -- * Object
    Object (..)

  -- * Game
  , Game (..)
  , gameShip
  , gameBounds
  , gameQuit

  -- * Controller
  , Controller (..)
  , controllerFlap
  , controllerRestart
  , controllerQuit

  -- * World
  ,  World (..)
  , worldScale
  , worldDims
  , worldFPS

  -- * Ship
  , Ship (..)
  , shipRect
  , shipV
  , shipVT
  , shipG
  , shipSprite

  -- * Extra Lenses
  , rectP
  , rectD
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Foreign.C.Types
import           SDL

data World = World
  { -- | The scale of the window in pixels/meter.
    _worldScale :: Double
  -- | The dimensions of the window in meters.
  , _worldDims  :: V2 Double
  -- | Game sampling rate.
  , _worldFPS   :: Double
  } deriving Show

data Ship = Ship
  { _shipRect   :: Rectangle Double
  , _shipV      :: Double
  , _shipVT     :: Double
  , _shipG      :: Double
  , _shipSprite :: FilePath
  } deriving Show

data Controller = Controller
  { _controllerFlap    :: Bool
  , _controllerRestart :: Bool
  , _controllerQuit    :: Bool
  } deriving Show

data Game = Game
  { _gameShip   :: Ship
  , _gameBounds :: V2 Double
  , _gameQuit   :: Bool
  } deriving Show

makeLenses ''Ship
makeLenses ''World
makeLenses ''Controller
makeLenses ''Game

class Object o where
  objRect :: o -> Rectangle Double

  objPRect :: World -> o -> Rectangle CInt
  objPRect w = fmap (round . (* w ^. worldScale)) . objRect

  objDraw :: MonadIO m => Renderer -> World -> o -> m ()
  objDraw r w = (rendererDrawColor r $= V4 255 255 0 255 >>)
    . drawRect r . Just . objPRect w

instance Object Ship where
  objRect = view shipRect
  objDraw r w ship = loadBMP (ship ^. shipSprite)
    >>= createTextureFromSurface r
    >>= \t -> copy r t Nothing (pure $ objPRect w ship)

instance Object Game where
  objRect = Rectangle (P $ V2 0 0) . view gameBounds
  objDraw r w game = do
    rendererDrawColor r $= V4 0 0 0 255
    clear r
    objDraw r w $ game ^. gameShip
    present r

rectP :: Lens' (Rectangle a) (Point V2 a)
rectP f (Rectangle p a) = flip Rectangle a <$> f p

rectD :: Lens' (Rectangle a) (V2 a)
rectD f (Rectangle p a) = Rectangle p <$> f a
