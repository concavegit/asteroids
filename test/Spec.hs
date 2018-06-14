import           Control.Lens
import           FRP.Yampa
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

prop_rectWithin :: Property
prop_rectWithin = property $ do
  -- pY <- double (Range.linearFrac -100 100)
  let r1 = -100
      r2 = 100
  r <-  (,) <$> forAll (Gen.double (Range.linearFrac r1 r2)) <*> forAll (Gen.double (Range.linearFrac r1 r2))

  pure ()
  -- pX <- forAll $ Gen.List (Range.linearFrac -100 100)
main :: IO ()
main = putStrLn "Test suite not yet implemented"
