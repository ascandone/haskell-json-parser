module Json.Parse2 (parseJson) where

import Control.Applicative (Alternative (some), optional)
import Control.Monad (void)
import Json.Internal (Json (..))
import ParsingCombinators (
  Parser,
  ParsingError,
  char,
  choice,
  digit,
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

-- >>> constructDigit [4, 2, 0]
-- 420
constructDigit :: [Int] -> Int
constructDigit = sum . zipWith construct [0 ..] . reverse
 where
  construct i digit = digit * (10 ^ i)

number :: Parser Float
number = do
  sign <- optional (char '-')
  res <- some digit
  let n = realToFrac $ constructDigit res
  case (res, sign) of
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
parseJson = parse json
