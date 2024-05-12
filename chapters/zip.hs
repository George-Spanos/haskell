cZip :: [a] -> [b] -> [(a, b)]
cZip [] _ = []
cZip _ [] = []
cZip (xa : a) (xb : b) = (xa, xb) : cZip a b

cZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cZipWith _ [] _ = []
cZipWith _ _ [] = []
cZipWith f (xa : a) (xb : b) = f xa xb : cZipWith f a b

cMerge :: [a] -> [b] -> [(a, b)]
cMerge = cZipWith (,)