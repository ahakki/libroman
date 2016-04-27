import           Control.Exception
import           Roman
import           Test.Hspec
import           Test.QuickCheck



main :: IO ()
main = hspec $ do
    describe "Roman.toRoman" $ do
        it "returns empty String for arabic zero" $
            toRoman 0 `shouldBe` ""

        it "returns I for arabic 1" $
            toRoman 1 `shouldBe` "I"

        it "returns II for arabic 2" $
            toRoman 2 `shouldBe` "II"
