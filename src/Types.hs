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

  -- * Mult
  , Mult (..)
  , multA
  , multB
  , multChoices

  -- * Asteroid
  , Asteroid (..)
  , genAsteroids
  , asteroidRect
  , asteroidV
  , asteroidNum
  , asteroidSprite
  , asteroidFont
  , asteroidColor

  -- * Extra Lenses
  , rectP
  , rectD
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Either
import           Data.Foldable
import           Data.Text              (pack)
import           Foreign.C.Types
import           FRP.Yampa
import           SDL
import           SDL.Font

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
  { _gameShip      :: Ship
  , _gameBounds    :: V2 Double
  , _gameMult      :: Mult
  , _gameAsteroids :: [Either Asteroid Asteroid]
  , _gameQuit      :: Bool
  }

data Ship = Ship
  { _shipRect   :: Rectangle Double
  , _shipV      :: Double
  , _shipVT     :: Double
  , _shipG      :: Double
  , _shipSprite :: Surface
  }

data Mult = Mult
  { _multA       :: Int
  , _multB       :: Int
  , _multChoices :: [Either Int Int]
  } deriving Show

data Asteroid = Asteroid
  { _asteroidRect   :: Rectangle Double
  , _asteroidV      :: Double
  , _asteroidNum    :: Int
  , _asteroidSprite :: Surface
  , _asteroidFont   :: Font
  , _asteroidColor  :: Color
  }

makeLenses ''Controller
makeLenses ''World
makeLenses ''Game
makeLenses ''Ship
makeLenses ''Mult
makeLenses ''Asteroid

class Object o where
  objRect :: o -> Rectangle Double

  objPRect :: World -> o -> Rectangle CInt
  objPRect w = fmap (round . (* w ^. worldScale)) . objRect

  objDraw :: MonadIO m => Renderer -> World -> o -> m ()
  objDraw r w = (rendererDrawColor r $= V4 255 255 0 255 >>)
    . drawRect r . Just . objPRect w

instance Object Game where
  objRect = Rectangle (P $ V2 0 0) . view gameBounds
  objDraw r w game = do
    rendererDrawColor r $= V4 0 0 0 255
    clear r
    traverse_ (objDraw r w . either id id) $ game ^. gameAsteroids
    objDraw r w $ game ^. gameShip
    present r

instance Object Ship where
  objRect = view shipRect
  objDraw r w ship = createTextureFromSurface r (ship ^. shipSprite)
    >>= \t -> copy r t Nothing (pure $ objPRect w ship)

instance Object Asteroid where
  objRect = view asteroidRect
  objDraw r w asteroid = do
    sprite <- createTextureFromSurface r $ asteroid ^. asteroidSprite
    copy r sprite Nothing (pure rect)

    num <- solid (asteroid ^. asteroidFont) (asteroid ^. asteroidColor) text
      >>= createTextureFromSurface r
    numD <- fmap fromIntegral . uncurry V2
      <$> size (asteroid ^. asteroidFont) text
    copy r num Nothing . Just
      $ Rectangle
      (rect ^. rectP
       + (P $ round . (/ 2) . fromIntegral <$> (rect ^. rectD - numD))
      ) numD

      where
        text = pack . show $ asteroid ^. asteroidNum
        rect = objPRect w asteroid

instance Random Mult where

  -- | Generate a random multiplication problem with a random amount
  -- of answer choices.
  random = runState $ do
    n <- rand
    a <- rand
    rand >>= randMultChoices n a

  -- | Generate random multiplication problem given boundaries as follows:
  --   * The multiplier boundaries are the square-roots of the problem answers.
  --   * The amount of answer choices is bounded by the amount of
  --     answer choices in the problems.
  randomR ms@(m1, m2) = runState $ do
    n <- randR (length $ m1 ^. multChoices, length $ m2 ^. multChoices)
    a <- randR r
    b <- randR r
    randMultChoices n a b

    where
      r = uncurry (***)
        (dup $ round . sqrt . fromIntegral . head . lefts . view multChoices) ms

rectP :: Lens' (Rectangle a) (Point V2 a)
rectP f (Rectangle p a) = flip Rectangle a <$> f p

rectD :: Lens' (Rectangle a) (V2 a)
rectD f (Rectangle p a) = Rectangle p <$> f a

rand :: (RandomGen g, Random a) => State g a
rand = state random

randR :: (RandomGen s, Random a) => (a, a) -> State s a
randR = state . randomR

randMultChoices :: RandomGen s => Int -> Int -> Int -> State s Mult
randMultChoices n a b = map (Left . (wrong !!))
  <$> replicateM (n - 1) (randR (0, length wrong - 1))
  >>= flip fmap (randR (0, n - 1))
  . ((Mult a b . ((. (Right ans:)) . (++) <$> uncurry take <*> uncurry drop)) .)
  . flip (,)

  where
    a' = div a 10
    b' = div b 10
    ans = a * b
    wrong = filter (/= ans)
      $ (+ (mod a 10 * mod b 10)) . (* 100) <$> [a' * b' .. (a' + 1) * (a' + 1)]

genAsteroids :: [Either Int Int] -> Double -> Double -> Double -> Surface -> Font -> Color -> [Either Asteroid Asteroid]
genAsteroids ns w h v sprite font color = (\(i, n) -> either (Left . f i) (Right . f i) $ n) <$> zip [0..] ns
  where
    d@(V2 s _) = uncurry V2 . dup $ h / fromIntegral (length ns)
    f i n = Asteroid
      { _asteroidRect = Rectangle (P . V2 w $ fromIntegral i * s) d
      , _asteroidV = v
      , _asteroidNum = n
      , _asteroidSprite = sprite
      , _asteroidFont = font
      , _asteroidColor = color
      }
