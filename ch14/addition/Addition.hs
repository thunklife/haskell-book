module Addition where

import Test.Hspec

main :: IO ()
main = hspec $ describe "Addition" $ do
  it "1 + 1 is greater than 1" $
    (1 + 1) > 1 `shouldBe` True
  it "2 + 2 is equal to 4" $
    (2 + 2) `shouldBe` 4