newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel = flip elem vowels

noOfVowels :: String -> Integer
noOfVowels [] = 0
noOfVowels (s : xs) = if isVowel s then 1 + noOfVowels xs else noOfVowels xs

mkWord :: String -> Maybe Word'
mkWord s =
  let v = noOfVowels s
      c = fromIntegral $ length s - fromIntegral v
   in if v > c then Nothing else Just $ Word' s