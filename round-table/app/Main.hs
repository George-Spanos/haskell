module Main (main) where

import Minesweeper.Web (runWebApp)

main :: IO ()
main = do
  putStrLn "Starting Minesweeper web server on port 3001..."
  runWebApp 3001
