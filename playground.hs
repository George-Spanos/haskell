f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

x = (+)

addLen :: String -> Int
addLen xs = w `x` 1
        where w = length xs

id :: * -> *
id x = x