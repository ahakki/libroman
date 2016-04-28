import           Control.Exception
import           Roman.Convert
import           Test.Hspec



main :: IO ()
main = hspec $
    describe "Roman.toRoman" $ do
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

        it "1929" $
            toRoman 1929 `shouldBe` "MCMXXIX"

        it "4999" $
            toRoman 4999 `shouldBe` "MMMMCMXCIX"

        it "42" $
            toRoman 42 `shouldBe` "XLII"

        it "96" $
            toRoman 96 `shouldBe` "XCVI"
