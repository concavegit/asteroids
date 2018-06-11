module Input
  ( senseController
  , senseDT
  ) where

import           Control.Lens
import           Data.IORef
import           SDL
import           Types

keyDown :: Keycode -> Event -> Bool
keyDown
  k
  (Event _ (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ e _))))
  = (k == e)
keyDown _ _ = False

handleEvent :: Event -> Controller -> Controller
handleEvent (Event _ QuitEvent) = controllerQuit .~ True
handleEvent e
  | keyDown KeycodeSpace e = controllerFlap .~ True
  | otherwise = id

senseController :: Controller -> IO Controller
senseController c = ($ c) . foldr (.) id . map handleEvent <$> pollEvents

senseDT :: IORef Double -> IO Double
senseDT tr = do
  t0 <- readIORef tr
  t <- (/ 1000) . fromIntegral <$> ticks
  writeIORef tr t
  pure $ t - t0
