module Hangman (runGame) where

import Data.Char (toLower)
import Data.List (intersperse)
import Debug.Trace
import GHC.Read (readField)
import System.Exit (exitSuccess)
import System.IO
import System.Random

letters = ['a' .. 'z']

minWordChars :: Int
minWordChars = 5

maxWordChars :: Int
maxWordChars = 9

errorTries = 5

includesSpecialCharacter :: String -> Bool
includesSpecialCharacter = foldr (\x y -> y || notElem x letters) False

allWords :: IO [String]
allWords = do
  contents <- readFile "data/dict.txt"
  return $ filter (not . includesSpecialCharacter) $ lines $ map toLower contents

getValidWords :: [String] -> [String]
getValidWords [] = []
getValidWords (a : xa)
  | length a > minWordChars && length a <= maxWordChars = a : getValidWords xa
  | otherwise = getValidWords xa

getRandomWord :: IO String
getRandomWord = do
  allWords <- allWords
  let validWords = getValidWords allWords
  rand <- randomRIO (0, length validWords)
  return $ validWords !! rand

type FoundLetters = [Char]

type HiddenWord = [Char]

data Puzzle = Puzzle FoundLetters HiddenWord

data GameStatus = Playing | Won | Lost

failedTries :: Puzzle -> Integer
failedTries (Puzzle [] b) = 0
failedTries (Puzzle a b) = foldr (\x y -> if x `notElem` b then y + 1 else y) 0 a

gameLost :: Puzzle -> Bool
gameLost = (> errorTries) . failedTries

gameWon :: Puzzle -> Bool
gameWon (Puzzle [] _) = False
gameWon (Puzzle a b) = foldr (\x y -> x `elem` a && y) True b

gameStatus :: Puzzle -> GameStatus
gameStatus (Puzzle [] _) = Playing
gameStatus p@(Puzzle a b)
  | gameWon p = Won
  | gameLost p = Lost
  | otherwise = Playing

gameLoop :: Puzzle -> IO (Maybe Puzzle)
gameLoop p@(Puzzle a b) = do
  let status = gameStatus p
  case status of
    Won -> do
      print $ "Won!!! " ++ b
      return Nothing
    Lost -> do 
      print $ "Lost. The word was " ++ b
      return Nothing
    Playing -> do
      logPuzzleInfo p
      hSetBuffering stdout NoBuffering
      putStr "Type a character: "
      c <- getLine
      let isValid = validateInput c
      if isValid
        then gameLoop $ Puzzle (head c : a) b
        else do
          putStrLn "please put one character"
          gameLoop p

validateInput :: [Char] -> Bool
validateInput x
  | length x > 1 = False
  | otherwise = True

logPuzzleInfo :: Puzzle -> IO Puzzle
logPuzzleInfo p@(Puzzle a _) = do
  putStrLn "---"
  print p
  putStrLn "---"
  putStrLn $ "Tried characters: " ++ a
  putStrLn $ "Failed Tries: " ++ show (failedTries p) ++ "/ " ++ show errorTries
  putStrLn "---"
  return p

instance Show Puzzle where
  show (Puzzle [] b) = unwords (map (const "_") b)
  show (Puzzle a b) = intersperse ' ' $ map (\x -> if x `elem` a then x else '_') b

runGame :: IO ()
runGame = do
  word <- getRandomWord
  gameLoop (Puzzle [] word)
  exitSuccess
  putStrLn "---"
  putStrLn "Game Ended"
  putStrLn "---"
