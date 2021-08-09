module Json.Parse2 (parseJson) where

import Control.Applicative (Alternative (some), optional)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Json.Internal (Json (..))
import ParsingCombinators (
  Parser,
  ParsingError,
  char,
  choice,
  digit,
  eof,
  fail,
  parse,
  string,
 )
import Prelude hiding (fail, null)

null :: Parser ()
null = void $ string "null"

boolean :: Parser Bool
boolean =
  choice
    "boolean"
    [ True <$ string "true"
    , False <$ string "false"
    ]

digits :: Parser [Int]
digits = some digit

-- >>> constructIntegerPart [4, 2, 0]
constructIntegerPart :: [Int] -> Int
constructIntegerPart = sum . zipWith construct [0 ..] . reverse
 where
  construct i digit = digit * (10 ^ i)

-- >>> constructFloatingPart [4, 2, 0, 3, 0]
-- 0.4203

-- >>> constructFloatingPart []
-- 0.0
constructFloatingPart :: [Int] -> Float
constructFloatingPart = sum . zipWith construct [1 ..]
 where
  construct i digit = (realToFrac digit) / (10 ^ i)

-- TODO exponent
number :: Parser Float
number = do
  sign <- optional (char '-')
  integerPart <- some digit
  fractionalPart <- optional $ do
    char '.'
    some digit

  let n = realToFrac (constructIntegerPart integerPart) + constructFloatingPart (fromMaybe [] fractionalPart)

  case (integerPart, sign) of
    (0 : _ : _, _) -> fail "a number different than zero" "0"
    (_, Just _) -> return (- n)
    (_, Nothing) -> return n

json :: Parser Json
json =
  choice
    "json"
    [ Null <$ null
    , Boolean <$> boolean
    , Number <$> number
    ]

parseJson :: String -> Either ParsingError Json
parseJson = parse (json <* eof)
