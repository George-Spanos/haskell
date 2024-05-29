{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}

module LearnApplicative (Identity, List) where

import Test.QuickCheck (Arbitrary (arbitrary), frequency)
import Test.QuickCheck.Checkers

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity b) = Identity (f b)

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap :: (a2 -> b) -> Constant a1 a2 -> Constant a1 b
  fmap _ (Constant b) = Constant b

instance (Monoid a) => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant a) <*> (Constant b) = Constant (a <> b)

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) $ fmap f b

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  -- (Cons f l) <*> (Cons a b) = Cons (f a) (l <*> b)
  (Cons f l) <*> as = fmap f as `append` (l <*> as)

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (2, Cons <$> arbitrary <*> arbitrary)]

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

-- take' :: Int -> List a -> List a
-- take' _ Nil = Nil
-- take' n (Cons a as)
--   | n <= 0 = Nil
--   | otherwise = Cons a (take' (n - 1) as)