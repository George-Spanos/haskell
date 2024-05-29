import Lib (JsonValue (..), parseJsonFile)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Member parse" $ do
    it "should parse member" $ do
      let expectedValue =
            JsonObject
              [ ("name", JsonString "John"),
                ("surname", JsonString "Smith"),
                ("age", JsonNumber 40),
                ("eyeColor", JsonString "blue")
              ]
      res <- parseJsonFile "input/member.json"
      case res of
        Nothing -> error "Failed to parse JSON"
        Just value -> value `shouldBe` expectedValue
    -- it "should parse families" $ do
    --   res <- parseJsonFile "input/families.json"
    --   case res of
    --     Nothing -> error "Failed to parse JSON"
    --     Just value -> value `shouldBe` expectedFamilies
    it "should parse simple array" $ do
      let expectedValue = JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3, JsonNull]
      res <- parseJsonFile "input/simple-array.json"
      case res of
        Nothing -> error "Failed to parse JSON"
        Just value -> value `shouldBe` expectedValue
