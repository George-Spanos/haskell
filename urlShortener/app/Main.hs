{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Function ((&))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty
  ( ScottyM,
    get,
    html,
    pathParam,
    queryParam,
    scotty,
    text,
  )

alphaNum :: String
alphaNum = ['A' .. 'Z'] ++ ['0' .. '9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex)
  return (xs !! randomDigit)

shortyGen :: IO [Char]
shortyGen = replicateM 7 (randomElement alphaNum)

saveURI ::
  R.Connection ->
  BC.ByteString ->
  BC.ByteString ->
  IO (Either R.Reply R.Status)
saveURI conn shortURI uri = R.runRedis conn $ R.set shortURI uri

linkShorty :: String -> String
linkShorty shorty = concat ["<a href=\"", shorty, "\">Copy and paste your short URL</a>"]

getURI ::
  R.Connection ->
  BC.ByteString ->
  IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

shortyCreated ::
  (Show a) =>
  a ->
  String ->
  TL.Text
shortyCreated resp shawty = TL.concat [TL.pack (show resp), " shorty is: ", TL.pack (linkShorty shawty)]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri = TL.concat [uri, " wasn't a url,", " did you forget http://?"]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs = TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

app :: R.Connection -> ScottyM ()
app rConn = do
  get "/" $ do
    uri <- queryParam "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Just _ -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
        let uri' = encodeUtf8 $ TL.toStrict uri
        resp <- liftIO (saveURI rConn shorty uri')
        html (shortyCreated resp shawty)
      Nothing -> text (shortyAintUri uri)
  get "/:short" $ do
    short <- pathParam "short"
    uri <- liftIO (getURI rConn short)
    case uri of
      Left r -> text (TL.pack (show r))
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just s -> s & decodeUtf8 & TL.fromStrict & shortyFound & html

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)