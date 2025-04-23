module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Tests" $ do
    it "should work" $ do
      True `shouldBe` True