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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

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
  { gameBoard :: Map Position CellState,  -- Renamed from 'board' to 'gameBoard' for clarity
    mines :: [Position],
    boardWidth :: Int,
    boardHeight :: Int,
    gameStatus :: GameStatus,
    revealedNonMineCount :: Int  -- Added to track revealed cells for efficient win detection
  }
  deriving (Show)

-- Create a new game with given width, height, and mine count
newGame :: Int -> Int -> Int -> Int -> GameState
newGame width height mineCount seed =
  let positions = [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
      minePositions = placeMines width height mineCount seed
      initialBoard = Map.fromList [(pos, Hidden) | pos <- positions]
   in GameState
        { gameBoard = initialBoard,
          mines = minePositions,
          boardWidth = width,
          boardHeight = height,
          gameStatus = InProgress,
          revealedNonMineCount = 0
        }

-- Randomly place mines on the board
placeMines :: Int -> Int -> Int -> Int -> [Position]
placeMines width height count seed =
  let gen = mkStdGen seed
      positions = [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
      -- Using shuffle' for more efficient and safer randomization
      shuffled = shuffle' positions (length positions) gen
   in take count shuffled

-- Get adjacent positions
getAdjacentPositions :: Int -> Int -> Position -> [Position]
getAdjacentPositions width height (x, y) =
  [ (nx, ny)
    | nx <- [max 0 (x - 1) .. min (width - 1) (x + 1)],
      ny <- [max 0 (y - 1) .. min (height - 1) (y + 1)],
      (nx, ny) /= (x, y)
  ]

-- Count adjacent mines using Set for more efficient lookup
countAdjacentMines :: [Position] -> Position -> Int -> Int -> Int
countAdjacentMines minePositions pos width height =
  let adjacentPositions = getAdjacentPositions width height pos
      mineSet = Set.fromList minePositions  -- Convert to Set for O(log n) lookups
   in length $ filter (`Set.member` mineSet) adjacentPositions

-- Reveal a cell with improved win detection using revealedNonMineCount
revealCell :: Position -> GameState -> GameState
revealCell pos gameState@GameState {gameBoard = board', mines = mines', boardWidth = width, 
                                   boardHeight = height, gameStatus = status, 
                                   revealedNonMineCount = revealed} =
  if status /= InProgress
    then gameState
    else case Map.lookup pos board' of
      Just Hidden ->
        if pos `elem` mines'  -- Consider using Set for mines too for larger boards
          then gameState {gameBoard = Map.insert pos RevealedMine board', gameStatus = Lost}
          else
            let adjacentMineCount = countAdjacentMines mines' pos width height
                newBoard = Map.insert pos (Revealed adjacentMineCount) board'
                newRevealed = revealed + 1
                gameState' = gameState {gameBoard = newBoard, revealedNonMineCount = newRevealed}
                
                -- If no adjacent mines, reveal neighbors recursively
                gameState'' =
                  if adjacentMineCount == 0
                    then revealNeighbors pos gameState'
                    else gameState'
                
                -- More efficient win detection using the counter
                totalNonMineCells = width * height - length mines'
                finalRevealed = revealedNonMineCount gameState''
                newStatus = if finalRevealed == totalNonMineCells then Won else InProgress
             in gameState'' {gameStatus = newStatus}
      Just Flagged -> gameState
      _ -> gameState

-- Reveal neighbors using a non-recursive approach to prevent stack overflow
revealNeighbors :: Position -> GameState -> GameState
revealNeighbors startPos gameState =
  let
    width = boardWidth gameState
    height = boardHeight gameState
    
    -- Helper function to process the queue of positions to reveal
    processQueue :: [Position] -> GameState -> GameState
    processQueue [] gs = gs
    processQueue (p:ps) gs =
      case Map.lookup p (gameBoard gs) of
        Just Hidden ->
          if p `elem` mines gs
            then processQueue ps gs  -- Skip mines
            else
              let adjacentMineCount = countAdjacentMines (mines gs) p width height
                  newBoard = Map.insert p (Revealed adjacentMineCount) (gameBoard gs)
                  newRevealed = revealedNonMineCount gs + 1
                  newGs = gs {gameBoard = newBoard, revealedNonMineCount = newRevealed}
                  
                  -- If cell has no adjacent mines, add its neighbors to the queue
                  newPs = if adjacentMineCount == 0
                         then ps ++ getAdjacentPositions width height p
                         else ps
               in processQueue newPs newGs
        _ -> processQueue ps gs  -- Skip non-hidden cells
    
    -- Initial queue with just the starting position's neighbors
    initialQueue = getAdjacentPositions width height startPos
  in
    processQueue initialQueue gameState

-- Flag or unflag a cell
flagCell :: Position -> GameState -> GameState
flagCell pos gameState@GameState {gameBoard = board', gameStatus = status} =
  if status /= InProgress
    then gameState
    else case Map.lookup pos board' of
      Just Hidden -> gameState {gameBoard = Map.insert pos Flagged board'}
      Just Flagged -> gameState {gameBoard = Map.insert pos Hidden board'}
      _ -> gameState

-- Check if the game is over
isGameOver :: GameState -> Bool
isGameOver GameState {gameStatus = status} = status /= InProgress

-- Check if the game is won
isGameWon :: GameState -> Bool
isGameWon GameState {gameStatus = status} = status == Won

-- Get the state of a cell
getCellState :: Position -> GameState -> Maybe CellState
getCellState pos GameState {gameBoard = board'} = Map.lookup pos board'

-- Get the entire board
getBoard :: GameState -> Map Position CellState
getBoard GameState {gameBoard = board'} = board'

-- Get the board size
getBoardSize :: GameState -> (Int, Int)
getBoardSize GameState {boardWidth = width, boardHeight = height} = (width, height)

-- Get the number of mines
getMineCount :: GameState -> Int
getMineCount GameState {mines = mines'} = length mines'