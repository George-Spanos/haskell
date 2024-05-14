module PlayWithArbitiary (main, Optional (Only, Nada), optionalAssoc, optGenString) where

import Data.Monoid
import Test.QuickCheck (Arbitrary (arbitrary), Gen, frequency)

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

optGen :: (Arbitrary a) => Gen (Optional a)
optGen = frequency [(1, return Nada), (3, Only <$> arbitrary)]

optGenString :: Gen (Optional String)
optGenString = optGen

instance (Semigroup a) => Semigroup (Optional a) where
  (<>) (Only a) (Only b) = Only $ a <> b
  (<>) _ (Only b) = Only b
  (<>) (Only a) _ = Only a
  (<>) Nada Nada = Nada

instance (Monoid a) => Monoid (Optional a) where
  mempty = Nada
  mappend = (<>)

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = optGen

optionalAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
optionalAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  let a = Only (Sum 1)
      b = Only (Sum 8)
      c = a <> b
  print c