module AnotherMaybe where

import PlayWithArbitiary (Optional (Nada, Only))
import Test.QuickCheck (Arbitrary (arbitrary))

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' (Only a)) _ = First' (Only a)
  (<>) _ (First' (Only b)) = First' (Only b)
  (<>) _ _ = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary

monoidAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
monoidAssoc a b c = ((a <> b) <> c) == (a <> (b <> c))

monoidLeftIdentity :: (Eq a) => a -> Bool
monoidLeftIdentity a = a == a

monoidRightIdentity :: Eq a => a -> Bool
monoidRightIdentity a = a == a
