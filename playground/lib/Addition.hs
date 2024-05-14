{-# OPTIONS_GHC -Wno-type-defaults #-}
module Addition (main, spec) where

import Test.Hspec
import Test.QuickCheck

add :: Integer -> Integer -> Integer
add a b = a + b

dividedBy :: (Integral a) => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise =
          go (n - d) d (count + 1)

main :: IO ()
main = print $ add 1 2

spec :: IO ()
spec = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it
      "22 divided by 5 is\
      \ 4 remainder 2"
      $ do
        dividedBy 22 5 `shouldBe` (4, 2)
    it
      "x + 1 is always\
      \ greater than x"
      $ do property $ \x -> x + 1 > (x :: Int)