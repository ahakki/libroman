import           Data.Roman
import           Test.Hspec
import           Test.QuickCheck



main :: IO ()
main = hspec $ do
   describe "Data.Roman" $
    it "fromInteger to Roman and back gives same Result" $
      property $
        \ x ->
          fromRoman (fromInteger x :: RomanList) == (x :: Integer) ||
            fromRoman (fromInteger (negate x) :: RomanList) ==
              (negate x :: Integer)

