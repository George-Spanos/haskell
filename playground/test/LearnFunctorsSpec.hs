module LearnFunctorsSpec (main) where

import Test.Hspec

main :: Spec
main = do
  describe "Learn Functors Spec" $ do
    it "should pass" $ do
      5 `shouldBe` 5