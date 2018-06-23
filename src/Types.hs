{-# LANGUAGE TemplateHaskell #-}

module Types
  ( -- * Object
    Object (..)

  -- * Game
  , Game (..)
  , gameShip
  , gameBounds
  , gameMultObj
  , gameAsteroidBelt
  , gameOver
  , gameMultRange
  , gameQuit
  , gameScore

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
  , shipSprite
  , shipG
  , shipV
  , shipVT

  -- * Mult
  , Mult (..)
  , multA
  , multB
  , multChoices

  -- * MultObj
  , MultObj (..)
  , multObjPos
  , multObjMult
  , multObjFont
  , multObjColor

  -- * Asteroid
  , Asteroid (..)
  , AsteroidBelt
  , asteroidSprite
  , asteroidV
  , asteroidVMult
  , asteroidNum
  , asteroidFont
  , asteroidColor
  , genAsteroids

  -- * Score
  , Score (..)
  , score
  , scorePoint
  , scoreFont
  , scoreColor

  -- * Sprite
  , Sprite (..)
  , sprite
  , spriteRect

  -- * Extra Lenses
  , rectP
  , rectD
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Either
import           Data.Foldable
import           Data.Text              (Text, pack)
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
  { _gameShip         :: Ship
  , _gameBounds       :: V2 Double
  , _gameMultObj      :: MultObj
  , _gameAsteroidBelt :: AsteroidBelt
  , _gameMultRange    :: (Mult, Mult)
  , _gameScore        :: Score
  , _gameOver         :: Bool
  , _gameQuit         :: Bool
  }

data Ship = Ship
  { _shipSprite :: Sprite
  , _shipV      :: Double
  , _shipVT     :: Double
  , _shipG      :: Double
  }

data Mult = Mult
  { _multA       :: Int
  , _multB       :: Int
  , _multChoices :: [Either Int Int]
  } deriving Show

data MultObj = MultObj
  { _multObjPos   :: Point V2 Double
  , _multObjMult  :: Mult
  , _multObjFont  :: Font
  , _multObjColor :: Color
  }

data Asteroid = Asteroid
  { _asteroidSprite :: Sprite
  , _asteroidV      :: Double
  , _asteroidVMult  :: Double
  , _asteroidNum    :: Int
  , _asteroidFont   :: Font
  , _asteroidColor  :: Color
  }

data Score = Score
  { _score      :: Int
  , _scorePoint :: Point V2 Double
  , _scoreFont  :: Font
  , _scoreColor :: Color
  }

data Sprite = Sprite
  { _spriteRect :: Rectangle Double
  , _sprite     :: Surface
  }

data TextBox = TextBox
  { _textBoxText  :: Text
  , _textBoxFont  :: Font
  , _textBoxColor :: Color
  , _textBoxPoint :: Point V2 Double
  }

type AsteroidBelt = [Either Asteroid Asteroid]

makeLenses ''Controller
makeLenses ''World
makeLenses ''Game
makeLenses ''Ship
makeLenses ''Mult
makeLenses ''MultObj
makeLenses ''Asteroid
makeLenses ''Score
makeLenses ''Sprite
makeLenses ''TextBox

class Object o where
  objRect :: o -> Rectangle Double

  objPRect :: World -> o -> Rectangle CInt
  objPRect w = fmap (round . (* w ^. worldScale)) . objRect

  objDraw :: MonadIO m => Renderer -> World -> o -> m ()
  objDraw r w = (rendererDrawColor r $= V4 255 255 0 255 >>)
    . drawRect r . pure . objPRect w

instance Object Sprite where
  objRect = view spriteRect
  objDraw r w o = createTextureFromSurface r (o ^. sprite)
    >>= \t -> copy r t mzero (pure $ objPRect w o)

instance Object TextBox where
  objRect = flip Rectangle (V2 0 0) . view textBoxPoint
  objDraw r w o = do
    txt <- solid (o ^. textBoxFont) (o ^. textBoxColor) (o ^. textBoxText) >>= createTextureFromSurface r
    dest <- Rectangle (objPRect w o ^. rectP) . fmap fromIntegral . uncurry V2 <$> size (o ^. textBoxFont) (o ^. textBoxText)
    copy r txt mzero $ pure dest

instance Object Game where
  objRect = Rectangle (P $ V2 0 0) . view gameBounds
  objDraw r w game = do
    rendererDrawColor r $= V4 0 0 0 255
    clear r
    traverse_ (objDraw r w . either id id) $ game ^. gameAsteroidBelt
    objDraw r w $ game ^. gameShip
    objDraw r w $ game ^. gameMultObj
    objDraw r w $ game ^. gameScore
    present r

instance Object Ship where
  objRect = objRect . view shipSprite
  objDraw r = (. view shipSprite) . objDraw r

instance Object MultObj where
  objRect o = Rectangle (o ^. multObjPos) $ V2 0 0
  objDraw r w o = objDraw r w TextBox
    { _textBoxText = pack
      $ (++) . (++ " × ") <$> show . view multA <*> show . view multB
      $ o ^. multObjMult
    , _textBoxFont = o ^. multObjFont
    , _textBoxColor = o ^. multObjColor
    , _textBoxPoint = o ^. multObjPos}

instance Object Asteroid where
  objRect = objRect . view asteroidSprite
  objDraw r w asteroid = do
    objDraw r w (asteroid ^. asteroidSprite)
    dims <- uncurry V2 . over both ((/ w ^. worldScale) . fromIntegral)
      <$> size (asteroid ^. asteroidFont) text
    objDraw r w TextBox
      { _textBoxText = text
      , _textBoxColor = asteroid ^. asteroidColor
      , _textBoxFont = asteroid ^. asteroidFont
      , _textBoxPoint = rect ^. rectP + (P $ (/ 2) <$> (rect ^. rectD - dims))
      }

      where
        text = pack . show $ asteroid ^. asteroidNum
        rect = objRect asteroid

instance Object Score where
  objRect = flip Rectangle (V2 0 0) . view scorePoint
  objDraw r w o = objDraw r w TextBox
    { _textBoxText = pack . show $ o ^. score
    , _textBoxColor = o ^. scoreColor
    , _textBoxFont = o ^. scoreFont
    , _textBoxPoint = o ^. scorePoint
    }

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
      r = ms & both %~ round . sqrt . fromIntegral . head . lefts . view multChoices

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
      $ (+ (mod a 10 * mod b 10)) . (* 10) <$> [a' * b' * 10 .. (a' + 1) * (b' + 1) * 10]

genAsteroids :: [Either Int Int] -> Double -> Asteroid -> AsteroidBelt
genAsteroids es h a =
  (\(i, n) -> either (Left . f i) (Right . f i) n) <$> zip [0..] es
  where
    d@(V2 s _) = uncurry V2 . dup $ h / fromIntegral (length es)
    f i n = execState
      ( asteroidSprite . spriteRect . rectP . _y .= i * s
      >> asteroidSprite . spriteRect . rectD .= d
      >> asteroidNum .= n
      ) a
