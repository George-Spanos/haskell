import Distribution.Types.MungedPackageId (computeCompatPackageId)
import GHC.Compact (compact)

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) =
  if x == False
    then False
    else myAnd xs

myOr :: [Bool] -> Bool
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = True
myAny f (x : xs) = f x || myAny f xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem a = myAny (== a)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a : xa) = myReverse xa ++ [a]

squish :: [[a]] -> [a]
squish [] = []
squish (a : xa) = a ++ squish xa

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [a] = a
myMaximumBy f (x : xs) =
  let mxs = myMaximumBy f xs
   in case f x mxs of
        GT -> x
        LT -> mxs
        EQ -> x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [a] = a
myMinimumBy f (x : xs) =
  let mxs = myMaximumBy f xs
   in case f x mxs of
        GT -> mxs
        LT -> x
        EQ -> mxs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare