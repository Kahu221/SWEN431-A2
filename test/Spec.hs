module Main where

import Test.Hspec

main :: IO ()
main = hspec $ describe "Example Test" $ do
    it "1 + 1 should equal 2" $ do
        (1 + 1) `shouldBe` 2