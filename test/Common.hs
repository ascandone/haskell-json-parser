module Common where

data Person = Person
  { name :: String
  , age :: Float
  , isDeveloper :: Maybe Bool
  }
  deriving (Show, Eq)

data Recursive = Recursive String [Recursive]
  deriving (Eq)
