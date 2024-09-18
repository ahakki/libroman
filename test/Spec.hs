--Spec.hs

import           Data.Roman
import           Test.Hspec
import           Test.QuickCheck

default (RomanNumeral)

main :: IO ()
main = hspec $ do
   describe "Data.Roman" $ do
    it "(fromRoman . fromInteger) = id" $ do
      property $
        \ x ->
             fromRoman (fromInteger x :: RomanNumeral) ==
             (x :: Integer)
          || fromRoman (fromInteger (negate x) :: RomanNumeral) ==
             (negate x :: Integer)

    it "(read . show) = id" $ do
      property $
        \ x ->
             ((read . show) (fromInteger x :: RomanNumeral) :: RomanNumeral) ==
             (fromInteger x :: RomanNumeral)

    it "Unconventionally Written Romans have equal value" $ do
             let unconvVal = fromRoman [X, I, I, X]    :: Integer
             let convVal =   fromRoman [X, V, I, I, I] :: Integer
             unconvVal `shouldBe` convVal

    it "(Unconventional Roman == Conventional Roman) fails" $ do
             let unconvVal = [X, I, I, X]
             let convVal   = [X, V, I, I, I]
             unconvVal == convVal `shouldBe` False
