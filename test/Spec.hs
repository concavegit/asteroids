{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Network.Extra
import           SDL
import           Types

newRect :: Applicative f => f a -> f a -> f a -> f a -> f (Rectangle a)
newRect mpx mpy mdx mdy = Rectangle <$> (P <$> (V2 <$> mpx <*> mpy))
  <*> (V2 <$> mdx <*> mdy)

-- | All rectangles with a vertex inside another are colliding with
-- the outer one.
prop_rectWithin :: Property
prop_rectWithin = property $ do
  outerRect <- newRect mG mG mG mG
  let p = outerRect ^. rectP
      px = p ^. _x
      py = p ^. _y
      x = forAll . Gen.double . Range.linearFrac px
        $ px + outerRect ^. rectD . _x
      y = forAll . Gen.double . Range.linearFrac py
        $ py + outerRect ^. rectD . _y
  innerRect <- newRect x y mG mG
  assert $ rectCollide innerRect outerRect
  where mG = forAll (Gen.double (Range.linearFrac 0 100))

-- | A is colliding with B iff B is colliding with A.
prop_collEq :: Property
prop_collEq = property $ do
  rect1 <- newRect mG mG mG mG
  rect2 <- newRect mG mG mG mG
  rectCollide rect1 rect2 === rectCollide rect2 rect1
  where mG = forAll (Gen.double (Range.linearFrac 0 100))

main :: IO ()
main = checkParallel $$discover >> pure ()
