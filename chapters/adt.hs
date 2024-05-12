{-# OPTIONS_GHC -Wno-missing-fields #-}

data Price = Price Integer deriving (Eq, Show)

type Seets = Integer

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Seets deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

-- areCars :: [Vehicle] -> Bool
-- areCars (x : xs) = isCar x || areCars xs

carList :: [Vehicle]
carList = [Car Mini (Price 20000), Car Mazda (Price 25000), Car Tata (Price 18000)]

vehicleList :: [Vehicle]
vehicleList = [Car Mini (Price 20000), Car Mazda (Price 25000), Car Tata (Price 18000), Plane PapuAir 10, Plane CatapultsR'Us 20]

areCars :: [Vehicle] -> Bool
areCars = foldr ((&&) . isCar) True

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car a _) = Just a
getManu _ = Nothing

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang = Haskell | Agda | Idris | PureScript deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux,
    OpenBSDPlusNevermindJustBSDStill,
    Mac,
    Windows
  ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

data Programmer = Programmer
  { os :: OperatingSystem,
    lang :: ProgLang
  }
  deriving (Eq, Show)

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

partialF :: Programmer
partialF =
  Programmer
    { os = Windows
    }