module ArbitiarySpec (main) where

import AnotherMaybe
    ( monoidAssoc,
      monoidLeftIdentity,
      monoidRightIdentity,
      FirstMappend,
      FstId )
import PlayWithArbitiary (Optional, optionalAssoc)
import Test.Hspec (describe, hspec, it)
import Test.QuickCheck (Testable (property))

type OptString = Optional String -> Optional String -> Optional String -> Bool

main :: IO ()
main = hspec $ do
  describe "Optional Monoid" $ do
    it "be associative" $ property (optionalAssoc :: OptString)
  describe "First Monoid" $ do
    it "should be associative" $ property (monoidAssoc :: FirstMappend)
    it "should prove left identity" $ property (monoidLeftIdentity :: FstId)
    it "should prove right identity" $ property (monoidRightIdentity :: FstId)