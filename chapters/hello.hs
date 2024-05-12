module Learn where
import Numeric (showInt)

(//) :: (Num a) => a -> a -> a
(//) a b = a + b + 2

customOp a b= a // b

main :: IO ()
main = print $ customOp 1 2