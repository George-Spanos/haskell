{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

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

data More a b
  = L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x)

fmap f (L a b a') = L (f a) b (f a')
fmap f (R b a b') = R b (f a) b'

main :: IO ()
main = do
  let a = Only (Sum 1)
      b = Only (Sum 8)
      c = a <> b
  print c