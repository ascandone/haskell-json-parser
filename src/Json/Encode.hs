module Json.Encode (
  number,
  boolean,
  string,
  object,
  array,
  null,
) where

import Data.Map (fromList)
import Json.Internal (Json (..))
import Prelude hiding (null)

number :: Float -> Json
number = Number

boolean :: Bool -> Json
boolean = Boolean

string :: String -> Json
string = String

array :: [Json] -> Json
array = Array

object :: [(String, Json)] -> Json
object = Object . fromList

null :: Json
null = Null
