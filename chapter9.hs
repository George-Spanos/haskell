import Data.Char

uppers :: String -> String
uppers [] = []
uppers x
  | isUpper $ head x = head x : uppers x
  | otherwise = uppers x

capitalize :: String -> String
capitalize [] = []
capitalize (xs : x) = toUpper xs : x

firstCapital :: String -> Char
firstCapital = toUpper . head