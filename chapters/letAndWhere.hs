module Main where

k = let x = 7
        y = negate x
        z = y * 10
    in z / x + y

j = z / x +y
  where
    x = 7
    y = negate x
    z = y * 10

main :: IO ()
main = print $ j == k