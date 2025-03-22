import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Arithmetic" $ do
    it "adds two numbers correctly" $ do
      (1 + 1) `shouldBe` 2

    it "multiplies two numbers correctly" $ do
      (2 * 3) `shouldBe` 6
