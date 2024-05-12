import Data.List (sort)

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

newtype Age
  = Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year
  = Year Integer
  deriving (Eq, Show)

sumNumberish :: (Numberish a) => a -> a -> a
sumNumberish a a' = fromNumber summed
  where
    integerOfA = toNumber a
    integerOfAPrime = toNumber a'
    summed = integerOfA + integerOfAPrime

i = 1

freud :: (Ord a) => a -> a
freud a = a

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)