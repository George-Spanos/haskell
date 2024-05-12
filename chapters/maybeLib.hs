mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = b
mayybee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a : _) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (a : xa) = case a of
  Just s -> s : catMaybes xa
  Nothing -> catMaybes xa

append :: a -> Maybe [a] -> Maybe [a]
append a Nothing = Nothing
append a (Just b) = Just $ a : b

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (x : xs) = case x of
  Nothing -> Nothing
  Just v -> append v $ flipMaybe xs