{-# LANGUAGE Arrows #-}

module Behavior
  ( shipFall
  , rectCollide
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           FRP.Yampa
import           SDL
import           Types

shipFall :: Ship -> SF Ship Ship
shipFall ship0 = proc ship -> do
  rec
    let a = g - v * g / vT
    v <- integral >>> iPre v0 -< a
    y <- integral -< v
  returnA -< execState (shipV .= y >> shipRect . rectP . _y .= y) ship

  where g = ship0 ^. shipG
        v0 = ship0 ^. shipV
        vT = ship0 ^. shipVT

rectCollide :: (Num t, Ord t) => Rectangle t -> Rectangle t -> Bool
rectCollide (Rectangle (P p1) d1) (Rectangle (P p2) d2) = within p2 (p1 + d1)
  && within (p1 - d2) p2
  where within = (and .) . liftA2 (<=)
