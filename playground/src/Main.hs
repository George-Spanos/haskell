module Main where

-- import qualified Addition as RE2
-- import qualified LearnReader as LR
-- import qualified RandomExample2 as RE (main)
-- import qualified LearnState as LS (main)
import qualified LearnParser as LR (main)
main :: IO ()
main = LR.main

-- Scotty example

-- {-# LANGUAGE OverloadedStrings #-}

-- import Web.Scotty

-- main:: IO ()
-- main = scotty 3000 $ do
--   get "/:word" $ do
--     beam <- captureParam "word"
--     html
--       ( mconcat
--           [ "<h1>Scotty, "
--           , beam
--           , " me up!</h1>"
--           ]
--       )
