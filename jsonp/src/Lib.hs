module Lib
  ( parseJsonFile,
    Parsable,
  )
where

import Control.Applicative ((<|>))
import Control.Exception
import qualified Control.Exception
import System.IO (readFile)
import Text.Trifecta

class Parsable a where
  parse :: String -> Maybe a

data JsonValue
  = JsonString String
  | JsonNumber Int
  | JsonObject [(String, JsonValue)]
  | JsonArray [JsonValue]
  | JsonBool Bool
  | JsonNull
  deriving (Show)

jsonString :: Parser String
jsonString = char '"' *> manyTill anyChar (char '"')

jsonInt :: Parser Int
jsonInt = do
  n <- some digit
  return (read n)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (oneOf " \n\t\r")

skipInitOrEndOrComa :: Parser ()
skipInitOrEndOrComa = skipOptional (char ',' <|> char '{' <|> char '}')

skipColon :: Parser ()
skipColon = skipOptional (char ':')

parseValue :: String -> JsonValue
parseValue = undefined

jsonKeyValue :: String -> Parser (String, JsonValue)
jsonKeyValue key = do
  _ <- skipInitOrEndOrComa
  _ <- skipWhitespace
  _ <- string $ concat ["\"", key, "\""]
  _ <- skipColon
  _ <- skipWhitespace
  value <- jsonString
  return (key, value)


parseContents :: (Parsable a) => String -> Maybe a
parseContents = undefined


safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile x = do
  result <- Control.Exception.try (readFile x) :: IO (Either Control.Exception.IOException String)
  case result of
    Left _ -> do
      print "File not found"
      return Nothing
    Right a -> return $ Just a

parseJsonFile :: (Parsable a) => FilePath -> IO (Maybe a)
parseJsonFile x = do
  content <- safeReadFile x
  return $ content >>= parseContents
