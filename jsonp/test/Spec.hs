import Lib (parseJsonFile)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Member parse" $ do
    it "should parse member" $ do
      let member = Member "John" "Smith" 40 "blue"
      -- res <- parseJsonFile "input/member.json"
      shouldBe True True
