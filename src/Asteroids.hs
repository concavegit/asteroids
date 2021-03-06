{-# LANGUAGE OverloadedStrings #-}

module Asteroids
  ( asteroids
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.IORef
import           FRP.Yampa
import           Input
import           Network
import           SDL
import qualified SDL.Font               as Font
import           System.Random
import           Types

asteroids :: World -> Game -> IO ()
asteroids world game = do
  initializeAll
  Font.initialize

  w <- createWindow "Asteroids" defaultWindow
    {windowInitialSize = round . (world ^. worldScale *) <$> world ^. worldDims}
  r <- createRenderer w (-1) defaultRenderer
  tr <- newIORef 0
  g <- getStdGen

  reactimate
    (pure controller)
    (\_ -> (. Just) . (,) <$> senseDT tr <*> senseController controller)
    (\_ -> render r world)
    (network g game)

  Font.quit
  quit

  where
    controller = Controller
      { _controllerFlap = False
      , _controllerRestart = False
      , _controllerQuit = False

      }
render :: MonadIO m => Renderer -> World -> Game -> m Bool
render r w g = do
  objDraw r w g
  SDL.delay (round $ 1000 / w ^. worldFPS)
  pure $ g ^. gameQuit
