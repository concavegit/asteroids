import           Control.Lens
import           FRP.Yampa
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Network
import           SDL
import           Types

prop_rectWithin :: Property
prop_rectWithin = property $ do
  -- pY <- double (Range.linearFrac -100 100)
  let mG = forAll (Gen.double (Range.linearFrac (-100) 100))
  outer <- newRect mG mG mG mG
  let p = outer ^. rectP
      px = p ^. _x
      py = p ^. _y
      x = forAll (Gen.double (Range.linearFrac px (px + outer ^. rectD . _x)))
      y = forAll (Gen.double (Range.linearFrac px (py + outer ^. rectD . _y)))
  inner <- newRect x y mG mG
  assert $ rectCollide outer inner

  pure ()
  where newRect mpx mpy mdx mdy = Rectangle <$> (P <$> (V2 <$> mpx <*> mpy)) <*> (V2 <$> mdx <*> mdy)
  -- pX <- forAll $ Gen.List (Range.linearFrac -100 100)
main :: IO ()
main = putStrLn "Test suite not yet implemented"
