module Lib
    ( someFunc
    , module Minesweeper.Game
    , module Minesweeper.Web
    ) where

import Minesweeper.Game
import Minesweeper.Web

someFunc :: IO ()
someFunc = putStrLn "Minesweeper Game"
