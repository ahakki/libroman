import           Control.Exception
import           Data.Word
import           Roman.Decode
import           Roman.Encode
import           Test.Hspec
import           Test.QuickCheck



main :: IO ()
main = hspec $ do
    describe "Roman.Encode" $ do
        it "0 -> \"\"" $
            toRoman 0 `shouldBe` ""

        it "1 -> I" $
            toRoman 1 `shouldBe` "I"

        it "2 -> II" $
            toRoman 2 `shouldBe` "II"

        it "4 -> IV" $
            toRoman 4 `shouldBe` "IV"

        it "6 -> VI" $
            toRoman 6 `shouldBe` "VI"

        it "14 -> XIV" $
            toRoman 14 `shouldBe` "XIV"

        it "1929 -> MCMXXIX" $
            toRoman 1929 `shouldBe` "MCMXXIX"

        it "4999 -> MMMMCMXCIX" $
            toRoman 4999 `shouldBe` "MMMMCMXCIX"

        it "42 -> XLII" $
            toRoman 42 `shouldBe` "XLII"

        it "96 -> XCVI" $
            toRoman 96 `shouldBe` "XCVI"

    describe "Roman.Decode" $
        it "converts romans to arabs" $ property $
            \x -> fromRoman (toRoman x) == (x :: Word16)
