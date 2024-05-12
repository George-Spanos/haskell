lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f x y = case x of
      Left a -> a : y
      Right v -> y

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f x y = case x of
      Left a -> y
      Right v -> v : y

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : iterate f (f a)