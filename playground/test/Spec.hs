import qualified LearnApplicativeSpec as LA (main)
-- import qualified LearnFunctorsSpec as LF (main)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  LA.main
