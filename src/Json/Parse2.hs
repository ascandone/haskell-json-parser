module Json.Parse2 (parseJson) where

import Control.Applicative (optional)
import Control.Monad (void)
import Json.Internal (Json (..))
import ParsingCombinators (
  Parser,
  ParsingError,
  choice,
  digit,
  fail,
  many,
  many1,
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
digits = uncurry (:) <$> many1 digit

-- >>> constructDigit [4, 2, 0]
-- 420
constructDigit :: [Int] -> Int
constructDigit = sum . zipWith construct [0 ..] . reverse
 where
  construct i digit = digit * (10 ^ i)

number :: Parser Float
number = do
  x <- optional (digit)
  res <- many1 (digit)
  case res of
    (0, _ : _) -> fail "a number different than zero" "0"
    (d, ds) -> return $ realToFrac $ constructDigit (d : ds)

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
