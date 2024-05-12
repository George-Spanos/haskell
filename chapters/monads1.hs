import Text.Read (readMaybe)

readInput :: IO (Either String Int)
readInput = do
  input <- getLine
  return $ case readMaybe input of 
    Just n -> Right n
    Nothing -> Left "Failed to Parse"
-- HOW ARE THESE TWO THE SAME?
-- readInput = do
--   maybe (Left "Failed to read") Right . readMaybe <$> getLine

increment :: Int -> Either String Int
increment 0 = Left "Zero"
increment x = Right (x + 1)

outcome :: Int -> String
outcome x = "Number: " ++ show x

printEval :: String -> IO ()
printEval = putStrLn . ("Result: " ++)
