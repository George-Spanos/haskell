import Data.List
import Distribution.TestSuite (Result)
import Data.Function ((&))

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

data DividedResult = Result Integer | DividedByZero deriving (Show)

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom
  | denom == 0 = DividedByZero
  | denom < 0 = case dividedBy num (-denom) of
      Result f -> Result $ negate f
      _ -> DividedByZero
  | otherwise = go num denom 0
  where
    go n d count
      | n < d = Result count
      | otherwise =
          go (n - d) d (count + 1)

sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n = n + sumTo (n - 1)

mc91 :: Integer -> Integer
mc91 x
  | x > 100 = x - 10
  | otherwise = 91

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits n = go n []
  where
    go n digs
      | d > 0 = go d (r : digs)
      | otherwise = r : digs
      where
        d = n `div` 10
        r = n `mod` 10

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits