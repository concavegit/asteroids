{-# LANGUAGE Arrows #-}

module Network.Extra
  ( -- * 'SF's
    edgeCond
  , switch1

  -- * Helper Functions
  , eitherFmap
  , gameQuitF
  , rectCollide
  ) where

import           Control.Applicative
import           Control.Lens
import           FRP.Yampa
import           SDL                 hiding (Event)
import           Types

-- | Set '_gameQuit' to '_controllerQuit'.
gameQuitF :: Controller -> Game -> Game
gameQuitF =  set gameQuit . view controllerQuit

-- | Map over an 'AsteroidBelt'.
eitherFmap :: Functor f => (a -> b) -> f (Either a a) -> f (Either b b)
eitherFmap = fmap . liftA2 either (Left .) (Right .)

-- | Check if two rectangles coincide.
rectCollide :: (Num t, Ord t) => Rectangle t -> Rectangle t -> Bool
Rectangle (P p1) d1 `rectCollide` Rectangle (P p2) d2 = within p2 (p1 + d1)
  && within (p1 - d2) p2
  where within = (and .) . liftA2 (<=)

-- | Given a condition, create a signal function outputting the input
-- with the event of the successful condition attached.
edgeCond :: (a -> Bool) -> SF a (a, Event a)
edgeCond f = dup ^>> second (dup ^>> second (f ^>> edge) >>^ uncurry (<$))

-- | A switch which reverts to the previous signal when the condition is over.
switch1 :: (a -> b -> SF c (d, Event e)) -> (e -> b) -> a -> b -> SF c d
switch1 g h f = flip switch (switch1 g h f . h) . g f
