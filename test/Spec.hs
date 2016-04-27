import           Control.Exception
import           Roman
import           Test.Hspec
import           Test.QuickCheck



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

        it "should convert arabic to roman" $
            filter (== False) (map (\(a, r) -> toRoman a == r) solutions) `shouldBe` []


solutions =
    [ (1, "I")
    , (2, "II")
    , (4, "IV")
    , (5, "V")
    , (1945, "MCMXLV")
    ]
