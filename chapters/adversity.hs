import Data.List (intercalate)
replaceThe :: String -> String
replaceThe s = unwords $ map (\x -> if isThe x then "a" else x) (words s)

isThe :: String -> Bool
isThe "the" = True
isThe _ = False

message = "the cow loves us"
message2 = "the oozing cow loves us the utermost the o the f"

vowels :: String
vowels = "aeiouy"

isVowel :: Char -> Bool
isVowel a = a `elem` vowels

nextIsVowel :: String -> Bool
nextIsVowel [] = False
nextIsVowel (s : xs) = isVowel s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel s =
    let (w:ws) = words s
        rest = unwords ws
    in if isThe w && nextIsVowel (head ws) then 1 + countTheBeforeVowel rest else countTheBeforeVowel rest


countVowels :: String -> Integer
countVowels [] = 0
countVowels (s:xs) = if isVowel s then 1+ countVowels xs else countVowels xs

main :: IO ()
main = do
  print $ countTheBeforeVowel message
  print $ countTheBeforeVowel message2
  print $ countVowels "Mikolajczak"
  print $ countVowels "the cow"