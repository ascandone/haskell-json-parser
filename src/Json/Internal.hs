module Json.Internal (Json (..)) where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

data Json
  = Number Float
  | Boolean Bool
  | String String
  | Array [Json]
  | Object (Map String Json)
  | Null
  deriving (Eq)

instance Show Json where
  show (Number x) = show x
  show (String x) = show x
  show (Array x) = "[" ++ intercalate ", " (map show x) ++ "]"
  show (Object x) = "{" ++ intercalate ", " (map showPair (Map.toList x)) ++ "}"
   where
    showPair (key, value) = show key ++ ": " ++ show value
  show (Boolean x) = if x then "true" else "false"
  show Null = "null"
