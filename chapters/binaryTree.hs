data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert a Leaf = Node Leaf a Leaf
insert a (Node l v r)
  | a < v = Node (insert a l) v r
  | a >= v = Node l v (insert a r)

mapTree :: (Ord a, Ord b) => (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf = Leaf
mapTree f (Node l v r) = Node (mapTree f l) (f v) (mapTree f r)

cTree :: BinaryTree Integer
cTree = (insert (-10) . insert 10 . insert 1 . insert 3 . insert 5) Leaf

isPositive :: (Num a, Ord a) => a -> Bool
isPositive x = x > 0

boolTree :: BinaryTree Bool
boolTree = mapTree isPositive cTree

toList :: (Ord a) => BinaryTree a -> [a]
toList Leaf = []
toList (Node l v r) = v : toList r ++ toList l

foldTree :: (a -> b -> b) -> BinaryTree a -> b -> b
foldTree _ Leaf b = b
foldTree f (Node l v r) b =
  let right = f v leftValue
      leftValue = foldTree f l b
   in foldTree f r right