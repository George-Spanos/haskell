-- {-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- module LearnState (main) where

-- import Control.Monad.State
-- import Debug.Trace (trace)

-- increment :: State Int Int
-- increment = do
--   n <- get
--   put (n + 1)
--   return n

-- expo :: State Int Int
-- expo = do
--   n <- get
--   put (10 ^ n)
--   return n

-- expoExample :: State Int Int
-- expoExample = do
--   expo
--   expo
--   get

-- example :: State Int Int
-- example = do
--   increment
--   increment
--   get

-- main :: IO ()
-- main = do
--   print $ runState example 0
--   print $ runState expoExample 0
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module LearnState (main) where

import Control.Monad.State

increment :: State Int Int
increment = do
  n <- get
  put (n + 1)
  return n

expo :: State Integer Integer
expo = do
  n <- get
  put 10
  return n

expoExample :: State Integer Integer
expoExample = do
  expo
  expo
  get

example :: State Int Int
example = do
  _ <- increment
  _ <- increment
  get

main :: IO ()
main = do
  print $ runState example 0
  print $ runState expoExample 2