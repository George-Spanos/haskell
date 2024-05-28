module LearnReader (main) where

import Data.Char
import Data.Monoid

newtype Reader r a = Reader {runReader :: r -> a}

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled x = (fmapped x, x)

myLiftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks f = Reader f

mySum :: (Num a) => a -> a -> a -> a
mySum a b c = a + b + c

main :: IO ()
main = do
  let res = mySum <$> Just 5 <*> Just 10 <*> Just 30
  let r = Just (Sum 5) <> Just (Sum 10) <> Nothing
  let k = [Just 1, Just 2, Nothing]
  let y = [10 .. 12]
  print $
    y
      <> foldl
        ( \acc v -> case v of
            Just x -> x : acc
            Nothing -> acc
        )
        []
        k
  print res
  print r
