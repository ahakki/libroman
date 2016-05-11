import           Control.Exception
import           Data.Roman
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck



main :: IO ()
main = hspec $ do
        describe "Data.Roman" $
            it "converts Lists of Roman Symbols to Integers" $ property $
                \x -> fromRoman (fromInteger x :: RomanList) == (x :: Integer) || (fromRoman ( fromInteger (negate x) :: RomanList ))  == (negate x :: Integer)

