module DomainTypes where

data Member = Member
  { name :: String,
    surname :: String,
    age :: Int,
    eyecolor :: String
  }
  deriving (Show)

data Family = Family
  { familyName :: String,
    member :: [Member]
  }
  deriving (Show)

newtype Families = Families
  { families :: [Family]
  }
  deriving (Show)
