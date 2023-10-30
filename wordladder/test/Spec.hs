import Test.Hspec
import Graph

main :: IO ()
main = hspec $ do
    describe "isLetterOff" $ do
        it "returns true if one letter off" $ do
            isLetterOff "slurs" "blurs" `shouldBe` (True :: Bool)
        
        it "returns false if more than one letter off" $ do
            isLetterOff "truth" "trust" `shouldBe` (False :: Bool)