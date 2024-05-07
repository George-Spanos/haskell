module FunctionWithLet where
printInc2 :: (Show a, Num a) => a -> IO ()
printInc2 n = let plusTwo = n + 2
              in print plusTwo