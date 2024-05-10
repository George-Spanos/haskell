import Data.Data (Data)
import Data.Time
import GHC.Exts (the)

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate
      UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)
      ),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate
      ( UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
      )
  ]

isDate :: DatabaseItem -> Bool
isDate x = case x of
  DbDate v -> True
  _ -> False

filterDbDate ::
  [DatabaseItem] ->
  [UTCTime]
filterDbDate = foldr concatDates []
  where
    concatDates (DbDate a) b = a : b
    concatDates _ b = b

filterDbNumber ::
  [DatabaseItem] ->
  [Integer]
filterDbNumber = foldr concatNumbers []
  where
    concatNumbers (DbNumber a) b = a : b
    concatNumbers _ b = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr reduceTime (UTCTime (ModifiedJulianDay 0) 0)
  where
    reduceTime (DbDate a) b = max a b
    reduceTime _ b = b

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr sum 0
  where
    sum (DbNumber a) b = a + b
    sum _ b = b

avgDb :: [DatabaseItem] -> Double
avgDb a = ((/ l) . fromInteger . sumDb) a
  where
    l = (fromIntegral . length . filterDbNumber) a

main :: IO ()
main = print $ avgDb theDatabase

-- filterDbDate ::
--   [DatabaseItem] ->
--   [UTCTime]
-- filterDbDate = map extractUTC filter isDate