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

        it "6 -> VI" $
            toRoman 6 `shouldBe` "VI"
