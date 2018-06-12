{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

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

  -- * Mult
  , Mult (..)
  , multA
  , multB
  , multAns

  -- * Extra Lenses
  , rectP
  , rectD
  ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Foreign.C.Types
import           SDL
import           System.Random

data Controller = Controller
  { _controllerFlap    :: Bool
  , _controllerRestart :: Bool
  , _controllerQuit    :: Bool
  } deriving Show

data World = World
  { -- | The scale of the window in pixels/meter.
    _worldScale :: Double
  -- | The dimensions of the window in meters.
  , _worldDims  :: V2 Double
  -- | Game sampling rate.
  , _worldFPS   :: Double
  } deriving Show

data Game = Game
  { _gameShip   :: Ship
  , _gameBounds :: V2 Double
  , _gameQuit   :: Bool
  } deriving Show

data Ship = Ship
  { _shipRect   :: Rectangle Double
  , _shipV      :: Double
  , _shipVT     :: Double
  , _shipG      :: Double
  , _shipSprite :: FilePath
  } deriving Show

data Mult = Mult
  { _multA   :: Int
  , _multB   :: Int
  , _multAns :: Int
  } deriving Show

makeLenses ''Controller
makeLenses ''World
makeLenses ''Game
makeLenses ''Ship
makeLenses ''Mult

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

instance Random Mult where
  random = runState $ newMult <$> rand <*> rand

  randomR ms = runState $ newMult <$> randR r <*> randR r
    where
      f = round . sqrt . fromIntegral . view multAns
      r = (f *** f) ms

rectP :: Lens' (Rectangle a) (Point V2 a)
rectP f (Rectangle p a) = flip Rectangle a <$> f p

rectD :: Lens' (Rectangle a) (V2 a)
rectD f (Rectangle p a) = Rectangle p <$> f a

rand :: (RandomGen g, Random a) => State g a
rand = state random

randR :: (RandomGen s, Random a) => (a, a) -> State s a
randR = state . randomR

newMult :: Int -> Int -> Mult
newMult a b = Mult
  { _multA = a
  , _multB = b
  , _multAns = a * b}
