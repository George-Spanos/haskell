{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Test.HUnit
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

import Minesweeper.Game

main :: IO ()
main = hspec $ do
  describe "Minesweeper.Game" $ do
    describe "newGame" $ do
      it "creates a new game with the correct dimensions" $ do
        let game = newGame 10 10 15 42
        getBoardSize game `shouldBe` (10, 10)
        getMineCount game `shouldBe` 15
        
      it "initializes all cells as hidden" $ do
        let game = newGame 5 5 5 42
            gameBoard = getBoard game
        all (== Hidden) (Map.elems gameBoard) `shouldBe` True
        
      it "places the correct number of mines" $ do
        let game = newGame 8 8 10 42
        length (mines game) `shouldBe` 10
    
    describe "flagCell" $ do
      it "flags a hidden cell" $ do
        let game = newGame 5 5 5 42
            pos = (1, 1)
            flaggedGame = flagCell pos game
            newState = getCellState pos flaggedGame
        newState `shouldBe` Just Flagged
        
      it "unflags a flagged cell" $ do
        let game = newGame 5 5 5 42
            pos = (1, 1)
            flaggedGame = flagCell pos game
            unflaggedGame = flagCell pos flaggedGame
            newState = getCellState pos unflaggedGame
        newState `shouldBe` Just Hidden
        
      it "doesn't change a revealed cell" $ do
        let game = newGame 5 5 5 42
            -- Ensure (2, 2) is not a mine for this test
            noMinePos = (2, 2)
            game' = if noMinePos `elem` mines game
                    then game { mines = filter (/= noMinePos) (mines game) }
                    else game
            revealedGame = revealCell noMinePos game'
            flaggedGame = flagCell noMinePos revealedGame
        getCellState noMinePos flaggedGame `shouldBe` getCellState noMinePos revealedGame
    
    describe "revealCell" $ do
      it "reveals a mine and loses the game" $ do
        let game = newGame 5 5 5 42
            minePositions = mines game
        case minePositions of
          [] -> fail "No mines in game"
          (minePos:_) -> do
            let newGame' = revealCell minePos game
            getCellState minePos newGame' `shouldBe` Just RevealedMine
            gameStatus newGame' `shouldBe` Lost
        
      it "reveals a non-mine cell" $ do
        let game = newGame 5 5 5 42
            nonMinePositions = [(x, y) | x <- [0..4], y <- [0..4], not ((x, y) `elem` mines game)]
        case nonMinePositions of
          [] -> fail "No non-mine positions available"
          (nonMinePos:_) -> do
            let newGame' = revealCell nonMinePos game
                state = fromJust $ getCellState nonMinePos newGame'
            case state of
              Revealed _ -> True `shouldBe` True
              _ -> assertFailure "Cell should be revealed with adjacent mine count"
      
      it "auto-reveals neighbors for cells with 0 adjacent mines" $ do
        let isRevealed (Revealed _) = True
            isRevealed _ = False
            -- For this test, we need to create a game with a known configuration
            game = GameState
                { board = Map.fromList [((x, y), Hidden) | x <- [0..2], y <- [0..2]]
                , mines = [(2, 2)]
                , boardWidth = 3
                , boardHeight = 3
                , gameStatus = InProgress
                }
            -- Reveal a cell with no adjacent mines
            game' = revealCell (0, 0) game
            -- Check if neighbors were also revealed
            revealed = 
                [ fromJust $ getCellState (0, 0) game'
                , fromJust $ getCellState (0, 1) game'
                , fromJust $ getCellState (1, 0) game'
                ]
        all isRevealed revealed `shouldBe` True

      it "maintains previously revealed cells when revealing new cells" $ do
        -- Create a test game with a known configuration
        let game = GameState
                { board = Map.fromList [((x, y), Hidden) | x <- [0..3], y <- [0..3]]
                , mines = [(3, 3)]  -- Only one mine in the corner
                , boardWidth = 4
                , boardHeight = 4
                , gameStatus = InProgress
                }
            -- First reveal operation at (0,0) - should cascade and reveal multiple cells
            game1 = revealCell (0, 0) game
            -- Check a few cells that should be revealed in the first operation
            posToCheck1 = [(0, 0), (0, 1), (1, 0)]
            
            -- All cells in posToCheck1 should be revealed in game1
            allRevealed1 = all (\pos -> 
                case getCellState pos game1 of
                  Just (Revealed _) -> True
                  _ -> False) posToCheck1
                
            -- Second reveal operation at (2,2) - should reveal more cells
            game2 = revealCell (2, 2) game1
            
            -- Check that the cells revealed in the first operation are still revealed
            stillRevealed = all (\pos -> 
                case getCellState pos game2 of
                  Just (Revealed _) -> True
                  _ -> False) posToCheck1
                
            -- And that the newly revealed cell is also revealed
            newlyRevealed = case getCellState (2, 2) game2 of
                  Just (Revealed _) -> True
                  _ -> False
                
        allRevealed1 `shouldBe` True
        stillRevealed `shouldBe` True
        newlyRevealed `shouldBe` True
  
  describe "Game Logic" $ do
    describe "isGameWon" $ do
      it "detects when a game is won" $ do
        -- Create a game with one mine
        let game = GameState
              { board = Map.fromList [((x, y), Hidden) | x <- [0..2], y <- [0..2]]
              , mines = [(2, 2)]
              , boardWidth = 3
              , boardHeight = 3
              , gameStatus = InProgress
              }
            -- Reveal all non-mine cells
            game' = foldl (flip revealCell) game [(x, y) | x <- [0..2], y <- [0..2], (x, y) /= (2, 2)]
        isGameWon game' `shouldBe` True
        
      it "detects when a game is not won" $ do
        let game = newGame 5 5 5 42
            nonMinePositions = [(x, y) | x <- [0..4], y <- [0..4], not ((x, y) `elem` mines game)]
        case nonMinePositions of
          [] -> fail "No non-mine positions available"
          (nonMinePos:_) -> do
            let game' = revealCell nonMinePos game
            isGameWon game' `shouldBe` False

    describe "isGameOver" $ do
      it "detects when a game is lost" $ do
        let game = newGame 5 5 5 42
            minePositions = mines game
        case minePositions of
          [] -> fail "No mines in game"
          (minePos:_) -> do
            let game' = revealCell minePos game
            isGameOver game' `shouldBe` True
        
      it "detects when a game is won" $ do
        -- Create a game with one mine
        let game = GameState
              { board = Map.fromList [((x, y), Hidden) | x <- [0..2], y <- [0..2]]
              , mines = [(2, 2)]
              , boardWidth = 3
              , boardHeight = 3
              , gameStatus = InProgress
              }
            -- Reveal all non-mine cells
            game' = foldl (flip revealCell) game [(x, y) | x <- [0..2], y <- [0..2], (x, y) /= (2, 2)]
        isGameOver game' `shouldBe` True
        
      it "detects when a game is in progress" $ do
        let game = newGame 5 5 5 42
        isGameOver game `shouldBe` False