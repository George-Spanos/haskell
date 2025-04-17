{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Minesweeper.Web
  ( runWebApp,
  )
where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Minesweeper.Game
import Network.Wai.Middleware.Static
import System.Random (randomIO)
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty hiding (status)
import Web.Scotty.Internal.Types (ActionError)

-- Run the web application
runWebApp :: Int -> IO ()
runWebApp port = do
  -- Create default game state with a random seed
  initialSeed <- randomIO
  let initialGame = newGame 10 10 15 initialSeed

  scotty port $ do
    -- Serve static files
    middleware $ staticPolicy (addBase "static")
    
    -- Home page
    get "/" $ do
      html $ renderHtml gamePage

    -- API routes
    get "/api/game" $ do
      json $ gameToJson initialGame

    post "/api/game/new" $ do
      -- Using formParam instead of deprecated param
      width <- formParam "width" `catch` (\(_ :: ActionError) -> return 10)
      height <- formParam "height" `catch` (\(_ :: ActionError) -> return 10)
      mineCount <- formParam "mines" `catch` (\(_ :: ActionError) -> return 15)

      gameSeed <- liftIO randomIO
      let newGameState = newGame width height mineCount gameSeed

      json $ gameToJson newGameState

    post "/api/game/reveal" $ do
      x <- formParam "x"
      y <- formParam "y"
      gameJsonText <- formParam "game" :: ActionM TL.Text
      
      let maybeGameJson = Aeson.decode (TL.encodeUtf8 gameJsonText)
          maybeGame = maybeGameJson >>= jsonToGame
      case maybeGame of
          Just game -> do
              let newGameState = revealCell (x, y) game
              json $ gameToJson newGameState
          Nothing -> 
              json $ Aeson.object ["error" .= ("Invalid game state" :: String)]

    post "/api/game/flag" $ do
      x <- formParam "x"
      y <- formParam "y"
      gameJsonText <- formParam "game" :: ActionM TL.Text
      
      let maybeGameJson = Aeson.decode (TL.encodeUtf8 gameJsonText)
          maybeGame = maybeGameJson >>= jsonToGame
      case maybeGame of
          Just game -> do
              let newGameState = flagCell (x, y) game
              json $ gameToJson newGameState
          Nothing -> 
              json $ Aeson.object ["error" .= ("Invalid game state" :: String)]

-- HTML for the game page
gamePage :: H.Html
gamePage = H.docTypeHtml $ do
    H.head $ do
        H.meta H.! A.charset "UTF-8"
        H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
        H.title "Haskell Minesweeper"
        H.link H.! A.rel "stylesheet" H.! A.href "/css/minesweeper.css"
    H.body $ do
        H.div H.! A.class_ "container" $ do
            H.h1 "Haskell Minesweeper"
            H.div H.! A.class_ "controls" $ do
                H.button H.! A.id "new-game" $ "New Game"
                H.span H.! A.id "mine-counter" $ "Mines: 0"
                H.span H.! A.id "game-status" $ "In Progress"
            H.div H.! A.id "board" $ ""
        H.script H.! A.src "/js/minesweeper.js" $ ""

-- Convert game state to JSON
gameToJson :: GameState -> Aeson.Value
gameToJson gameState =
    let (width, height) = getBoardSize gameState
        mineCount = getMineCount gameState
        cells = Map.toList $ getBoard gameState
        status = if isGameOver gameState 
                 then if isGameWon gameState then ("won" :: String) else ("lost" :: String)
                 else ("in-progress" :: String)
    in Aeson.object
        [ "width" .= width
        , "height" .= height
        , "mineCount" .= mineCount
        , "status" .= status
        , "cells" .= Aeson.object [ Key.fromText (T.pack (show x ++ "," ++ show y)) .= cellToJson state 
                                   | ((x, y), state) <- cells
                                   ]
        ]

-- Convert cell state to JSON
cellToJson :: CellState -> Aeson.Value
cellToJson Hidden = Aeson.object [ "type" .= ("hidden" :: String) ]
cellToJson Flagged = Aeson.object [ "type" .= ("flagged" :: String) ]
cellToJson (Revealed n) = Aeson.object 
    [ "type" .= ("revealed" :: String)
    , "adjacentMines" .= n
    ]
cellToJson RevealedMine = Aeson.object [ "type" .= ("mine" :: String) ]

-- Convert JSON to game state (simplified for this example)
jsonToGame :: Aeson.Value -> Maybe GameState
jsonToGame jsonVal = do
  obj <- case jsonVal of
    Aeson.Object o -> Just o
    _ -> Nothing
  
  width <- AesonTypes.parseMaybe (.: "width") obj
  height <- AesonTypes.parseMaybe (.: "height") obj
  mineCount <- AesonTypes.parseMaybe (.: "mineCount") obj
  
  -- Use a fixed seed for simplicity
  return $ newGame width height mineCount 42