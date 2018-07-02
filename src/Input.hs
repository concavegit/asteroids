-- | This module offers input detection for the game.

module Input
  ( senseController
  , senseDT
  ) where

import           Control.Lens
import           Data.IORef
import           SDL
import           Types

-- | Return a 'Bool' corresponding to whether or not the specified
-- 'KeyCode' has been pressed.
keyDown :: Keycode -> Event -> Bool
keyDown
  k
  (Event _ (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ e _))))
  = k == e
keyDown _ _ = False

handleEvent :: Event -> Controller -> Controller
handleEvent (Event _ QuitEvent) = controllerQuit .~ True
handleEvent e
  | keyDown KeycodeSpace e = controllerFlap .~ True
  | otherwise = id

-- | Modify an initial 'Controller' based on input.
senseController :: Controller -> IO Controller
senseController c = ($ c) . foldr ((.) . handleEvent) id <$> pollEvents

-- | Sense the amount of time delta between samples for the FRP
-- network.
senseDT :: IORef Double -> IO Double
senseDT tr = do
  t0 <- readIORef tr
  t <- (/ 1000) . fromIntegral <$> ticks
  writeIORef tr t
  pure $ t - t0
