import           Control.Exception
import           Data.Word8
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

    describe "Roman.Decode" $ do
        it "I -> 1" $
            fromRoman "I" `shouldBe` 1

        it "II -> 2" $
            fromRoman "II" `shouldBe` 2

        it "IV -> 4" $
            fromRoman "IV" `shouldBe` 4

        it "converts romans to arabs" $ property $
            \x -> fromRoman (toRoman x) == (x :: Word8)
