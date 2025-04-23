module Minesweeper.Game
  ( GameState (..),
    CellState (..),
    GameStatus (..),
    Position,
    newGame,
    revealCell,
    flagCell,
    isGameOver,
    isGameWon,
    getCellState,
    getBoard,
    getBoardSize,
    getMineCount,
  )
where

import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Random (mkStdGen, randomRs)

-- Cell position on the board
type Position = (Int, Int)

-- Number of adjacent mines for a cell
type AdjacentMines = Int

-- Define cell state
data CellState
  = Hidden
  | Flagged
  | Revealed AdjacentMines
  | RevealedMine
  deriving (Show, Eq)

-- Game status
data GameStatus
  = InProgress
  | Won
  | Lost
  deriving (Show, Eq)

-- Game state
data GameState = GameState
  { board :: Map Position CellState,
    mines :: [Position],
    boardWidth :: Int,
    boardHeight :: Int,
    gameStatus :: GameStatus
  }
  deriving (Show)

-- Create a new game with given width, height, and mine count
newGame :: Int -> Int -> Int -> Int -> GameState
newGame width height mineCount seed =
  let positions = [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
      minePositions = placeMines width height mineCount seed
      initialBoard = Map.fromList [(pos, Hidden) | pos <- positions]
   in GameState
        { board = initialBoard,
          mines = minePositions,
          boardWidth = width,
          boardHeight = height,
          gameStatus = InProgress
        }

-- Randomly place mines on the board
placeMines :: Int -> Int -> Int -> Int -> [Position]
placeMines width height count seed =
  let gen = mkStdGen seed
      positions = [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
      indices = take count $ nub $ randomRs (0, width * height - 1) gen
   in map (positions !!) indices

-- Get adjacent positions
getAdjacentPositions :: Int -> Int -> Position -> [Position]
getAdjacentPositions width height (x, y) =
  [ (nx, ny)
    | nx <- [max 0 (x - 1) .. min (width - 1) (x + 1)],
      ny <- [max 0 (y - 1) .. min (height - 1) (y + 1)],
      (nx, ny) /= (x, y)
  ]

-- Count adjacent mines
countAdjacentMines :: [Position] -> Position -> Int -> Int -> Int
countAdjacentMines minePositions pos width height =
  let adjacentPositions = getAdjacentPositions width height pos
   in length $ filter (`elem` minePositions) adjacentPositions

-- Reveal a cell
revealCell :: Position -> GameState -> GameState
revealCell pos gameState@GameState {board = board', mines = mines', boardWidth = width, boardHeight = height, gameStatus = status} =
  if status /= InProgress
    then gameState
    else case Map.lookup pos board' of
      Just Hidden ->
        if pos `elem` mines'
          then gameState {board = Map.insert pos RevealedMine board', gameStatus = Lost}
          else
            let adjacentMineCount = countAdjacentMines mines' pos width height
                newBoard = Map.insert pos (Revealed adjacentMineCount) board'
                gameState' = gameState {board = newBoard}
                -- If no adjacent mines, reveal neighbors recursively
                gameState'' =
                  if adjacentMineCount == 0
                    then revealNeighbors pos gameState'
                    else gameState'
                -- Check if the game is won
                remainingHidden = length $ filter isHiddenCell $ Map.toList $ board gameState''
                newStatus = if remainingHidden == length mines' then Won else InProgress
             in gameState'' {gameStatus = newStatus}
      Just Flagged -> gameState
      _ -> gameState
  where
    isHiddenCell (_, Hidden) = True
    isHiddenCell (_, Flagged) = True
    isHiddenCell _ = False

-- Reveal neighbors recursively when a cell with 0 adjacent mines is revealed
revealNeighbors :: Position -> GameState -> GameState
revealNeighbors pos gameState@GameState {board = board', boardWidth = width, boardHeight = height} =
  let neighbors = getAdjacentPositions width height pos
      hiddenNeighbors = filter (\p -> Map.lookup p board' == Just Hidden) neighbors
   in foldl (flip revealCell) gameState hiddenNeighbors

-- Flag or unflag a cell
flagCell :: Position -> GameState -> GameState
flagCell pos gameState@GameState {board = board', gameStatus = status} =
  if status /= InProgress
    then gameState
    else case Map.lookup pos board' of
      Just Hidden -> gameState {board = Map.insert pos Flagged board'}
      Just Flagged -> gameState {board = Map.insert pos Hidden board'}
      _ -> gameState

-- Check if the game is over
isGameOver :: GameState -> Bool
isGameOver GameState {gameStatus = status} = status /= InProgress

-- Check if the game is won
isGameWon :: GameState -> Bool
isGameWon GameState {gameStatus = status} = status == Won

-- Get the state of a cell
getCellState :: Position -> GameState -> Maybe CellState
getCellState pos GameState {board = board'} = Map.lookup pos board'

-- Get the entire board
getBoard :: GameState -> Map Position CellState
getBoard GameState {board = board'} = board'

-- Get the board size
getBoardSize :: GameState -> (Int, Int)
getBoardSize GameState {boardWidth = width, boardHeight = height} = (width, height)

-- Get the number of mines
getMineCount :: GameState -> Int
getMineCount GameState {mines = mines'} = length mines'