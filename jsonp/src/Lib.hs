module Lib
  ( parseJsonFile,
    parseContents,
    JsonValue (..),
  )
where

import Control.Applicative ((<|>))
import Control.Exception
import Data.Functor (($>))
import Text.Trifecta

data JsonValue
  = JsonString String
  | JsonNumber Int
  | JsonBool Bool
  | JsonNull
  | JsonObject [(String, JsonValue)]
  | JsonArray [JsonValue]
  deriving (Show, Eq)

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
  _ <- skipSuroundingWhiteSpace (char '[')
  values <- skipSuroundingWhiteSpace jsonValue `sepBy` skipSuroundingWhiteSpace (char ',')
  _ <- skipSuroundingWhiteSpace (char ']')

  return $ JsonArray values

jsonObject :: Parser JsonValue
jsonObject = do
  _ <- skipSuroundingWhiteSpace (char '{')
  value <- jsonKeyValue `sepBy` skipSuroundingWhiteSpace (char ',')
  _ <- skipSuroundingWhiteSpace (char '}')
  return $ JsonObject value

jsonValue :: Parser JsonValue
jsonValue =
  jsonString
    <|> jsonInt
    <|> jsonBool
    <|> jsonNull
    <|> jsonArray
    <|> jsonObject

skipWhitespace :: Parser ()
skipWhitespace = skipMany (oneOf " \n\t\r")

skipSuroundingWhiteSpace :: Parser a -> Parser a
skipSuroundingWhiteSpace a = skipWhitespace *> a <* skipWhitespace

skipInitOrEndOrComa :: Parser ()
skipInitOrEndOrComa = skipOptional (char ',' <|> char '{' <|> char '}')

skipColon :: Parser ()
skipColon = skipOptional (char ':')

jsonKeyValue :: Parser (String, JsonValue)
jsonKeyValue = do
  _ <- skipInitOrEndOrComa
  (JsonString s) <- skipSuroundingWhiteSpace jsonString
  _ <- skipColon
  value <- skipSuroundingWhiteSpace jsonValue
  return (s, value)

parseContents :: String -> Maybe JsonValue
parseContents input =
  case parseString (skipWhitespace *> jsonValue <* eof) mempty input of
    Success result -> Just result
    Failure _ -> Nothing

safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile x = do
  result <- Control.Exception.try (readFile x) :: IO (Either Control.Exception.IOException String)
  case result of
    Left _ -> do
      print "File not found"
      return Nothing
    Right a -> return $ Just a

parseJsonFile :: FilePath -> IO (Maybe JsonValue)
parseJsonFile x = do
  content <- safeReadFile x
  return $ content >>= parseContents
