eft :: Integer -> Integer -> [Integer]
eft a b
  | a > b = []
  | a == b = [a]
  | otherwise = a : eft (a + 1) b

extractWords :: String -> [String]
extractWords [] = []
extractWords w = fWord : extractWords b
  where
    fWord = takeWhile (/= ' ') w
    len = length fWord
    b = drop (len + 1) w

filterMultThree :: [Integer]
filterMultThree = filter (\x -> rem x 3 == 0) [1 .. 30]

isArticle :: String -> Bool
isArticle x
  | x == "an" = True
  | x == "a" = True
  | x == "the" = True
  | otherwise = False

filterArticles :: String -> [String]
filterArticles [] = []
filterArticles x = filter (not . isArticle) $ words x