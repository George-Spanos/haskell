{-# LANGUAGE OverloadedStrings #-}

module LearnState (run) where

import Control.Monad.State (State, get, put, runState)
import Data.Text (Text, singleton)

appendChar :: Char -> State Text Text
appendChar c = do
  name <- get
  put "manolis"
  return $ name <> singleton c

run :: IO ()
run = do
  let s = runState (appendChar 's') "Initial State"
  print s