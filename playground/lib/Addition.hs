module Addition (sayHello, add) where

  sayHello :: IO ()
  sayHello = putStrLn "hello!"
  
  add :: Integer -> Integer -> Integer
  add a b = a + b