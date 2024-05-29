module ValueParser (JsonValue) where

import Text.Trifecta

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


parseValue :: String -> JsonValue
parseValue = undefined