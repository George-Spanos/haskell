module Cipher where

import Data.Char
import Data.List (elemIndex)
import Distribution.Compat.CharParsing (letter)

message = "Meet me at the park at midnight. zz"

cipheredMessage = "Phhw ph dw wkh sdun dw plgqljkw. cc"

letters = ['a' .. 'z']

caps = ['A' .. 'Z']

rollLetter :: Integer -> Char -> Char
rollLetter offset c
  | c `elem` letters =
      let a = elemIndex c letters
       in case a of
            Just v -> letters !! mod (v + fromInteger offset) 26
            Nothing -> ' '
  | c `elem` caps =
      let a = elemIndex c caps
       in case a of
            Just v -> caps !! mod (v + fromInteger offset) 26
            Nothing -> ' '
  | otherwise = c

cipher :: Integer -> String -> String
cipher _ [] = []
cipher 0 s = s
cipher offset (s : xs) = rollLetter offset s : cipher offset xs

main :: IO ()
main = do
  print (cipher 3 message == cipheredMessage)
  print (cipher (-3) cipheredMessage == message)