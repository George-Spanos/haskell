module Lib
  ( parseJsonFile,
    Parsable,
  )
where

import Control.Applicative ((<|>))
import Control.Exception
import qualified Control.Exception
import Data.Functor (($>))
import System.IO (readFile)
import Text.Trifecta

class Parsable a where
  parse :: String -> Maybe a

data JsonValue
  = JsonString String
  | JsonNumber Int
  | JsonBool Bool
  | JsonNull
  | JsonObject [(String, JsonValue)]
  | JsonArray [JsonValue]
  deriving (Show)

jsonString :: Parser JsonValue
jsonString = fmap JsonString $ char '"' *> manyTill anyChar (char '"')

jsonInt :: Parser JsonValue
jsonInt = do
  n <- some digit
  (return . JsonNumber . read) n

jsonBool :: Parser JsonValue
jsonBool =
  choice
    [ JsonBool True <$ string "true",
      JsonBool False <$ string "false"
    ]

jsonNull :: Parser JsonValue
jsonNull = string "null" $> JsonNull

jsonArray :: Parser JsonValue
jsonArray = do
  _ <- char '['
  values <- jsonValue `sepBy` char ','
  _ <- char ']'
  return $ JsonArray values

jsonObject :: Parser JsonValue
jsonObject = do
  _ <- char '{'
  pairs <- keyValuePair `sepBy` char ','
  _ <- char '}'
  return $ JsonObject pairs
  where
    keyValuePair = do
      key <- jsonString
      _ <- char ':'
      value <- jsonValue
      case key of
        JsonString k -> return (k, value)
        _ -> fail "Expected a JSON string as key"

jsonValue :: Parser JsonValue
jsonValue =
  choice
    [ jsonString,
      jsonInt,
      jsonBool,
      jsonNull,
      jsonArray,
      jsonObject
    ]

skipWhitespace :: Parser ()
skipWhitespace = skipMany (oneOf " \n\t\r")

skipInitOrEndOrComa :: Parser ()
skipInitOrEndOrComa = skipOptional (char ',' <|> char '{' <|> char '}')

skipColon :: Parser ()
skipColon = skipOptional (char ':')

-- jsonKeyValue :: String -> Parser (String, JsonValue)
-- jsonKeyValue key = do
--   _ <- skipInitOrEndOrComa
--   _ <- skipWhitespace
--   _ <- string $ concat ["\"", key, "\""]
--   _ <- skipColon
--   _ <- skipWhitespace
--   value <- jsonString
--   return (key, value)

parseContents :: (Parsable a) => String -> Maybe a
parseContents = undefined

-- parseContents = do
--   _ <- skipInitOrEndOrComa
--   _ <- skipWhitespace
--   (JsonString s) <- jsonString

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
