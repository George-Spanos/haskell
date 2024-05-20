module LearnApplicativeSpec (main) where

import LearnApplicative (List)
import Test.Hspec
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: Spec
main = do
  let trigger = undefined :: List (Int, String, List String)
  describe "List Spec" $ do
    it "Functor" $ quickBatch (functor trigger)
    it "Applicative" $ quickBatch $ applicative trigger