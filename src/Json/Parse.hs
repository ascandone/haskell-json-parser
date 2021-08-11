module Json.Parse (
  parseJson,
  ParsingError,
) where

import Control.Applicative (Alternative (many, some, (<|>)), optional)
import Control.Monad (void)
import Data.Char (chr, isHexDigit)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Json.Internal (Json (..))
import ParsingCombinators (
  Parser,
  ParsingError,
  any,
  between,
  char,
  choice,
  digit,
  eof,
  fail,
  hexDigit,
  parse,
  satisfy,
  sepBy,
  string,
  try,
 )
import Prelude hiding (fail, null)

null :: Parser ()
null = void $ ParsingCombinators.string "null"

boolean :: Parser Bool
boolean =
  choice
    "boolean"
    [ True <$ ParsingCombinators.string "true"
    , False <$ ParsingCombinators.string "false"
    ]

digits :: Parser [Int]
digits = some (try digit)

-- >>> constructIntegerPart [4, 2, 0]
constructIntegerPart :: Int -> [Int] -> Int
constructIntegerPart base = sum . zipWith construct [0 ..] . reverse
 where
  construct i digit = digit * (base ^ i)

-- >>> constructFloatingPart [4, 2, 0, 3, 0]
-- 0.4203

-- >>> constructFloatingPart []
-- 0.0
constructFloatingPart :: [Int] -> Float
constructFloatingPart = sum . zipWith construct [1 ..]
 where
  construct i digit = (realToFrac digit) / (10 ^ i)

-- >>> parse number "42.2"
-- Right 42.2

-- TODO exponent
number :: Parser Float
number = do
  sign <- optional (char '-')
  integerPart <- digits
  fractionalPart <- optional $ do
    char '.'
    digits

  let n = realToFrac (constructIntegerPart 10 integerPart) + constructFloatingPart (fromMaybe [] fractionalPart)

  case (integerPart, sign) of
    (0 : _ : _, _) -> fail "a number different than zero" "0"
    (_, Just _) -> return (- n)
    (_, Nothing) -> return n

whitespace :: Parser ()
whitespace =
  void $
    choice
      "whitespace"
      [ char ' '
      , char '\n'
      , char '\t'
      , char '\r'
      ]

array :: Parser [Json]
array = between (char '[') (char ']') (json `sepBy` separator)
 where
  separator = many whitespace >> char ',' >> many whitespace

unicode :: Parser Char
unicode = do
  a <- hexDigit
  b <- hexDigit
  c <- hexDigit
  d <- hexDigit
  let num = constructIntegerPart 16 [a, b, c, d]
  return (chr num)

escapeChar :: Parser Char
escapeChar = do
  ch <- ParsingCombinators.any
  case ch of
    '"' -> return '\"'
    '\\' -> return '\\'
    '/' -> return '/'
    'b' -> return '\b'
    'f' -> return '\f'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    'u' -> unicode
    _ -> fail "expected escape char" [ch]

-- >>> parse Json.Parse.string "\"\""
-- Right ""

-- >>> parse Json.Parse.string "\"hello\""
-- Right "hello"

-- >>> parse Json.Parse.string "\"he\nllo\""
-- Right "he\nllo"
string :: Parser String
string = between (char '"') (char '"') $
  many $
    try $ do
      ch <- ParsingCombinators.any
      case ch of
        '"' -> fail "expected char" "\""
        '\\' -> escapeChar
        _ -> return ch

-- >>> parse object "{}"
-- Right []

-- >>> parse object "{\"key\":42}"
-- Right [("key",42.0)]

-- >>> parse object "{  \"key\"  :  42  }"
-- Right [("key",42.0)]

-- >>> parse object "{  \"key\"  :  42  , \"k2\": \"hi\" }"
-- Right [("key",42.0),("k2","hi")]

-- >>> parse parser "{  \"x\"  \t :  0  ,\n \"y\":1}"
-- Right {"x": 0.0, "y": 1.0}

object :: Parser [(String, Json)]
object = between (char '{') (many whitespace <* char '}') (kw `sepBy` separator)
 where
  separator = many whitespace >> char ',' >> many whitespace
  kw = do
    many whitespace
    key <- Json.Parse.string
    many whitespace
    char ':'
    many whitespace
    value <- json
    many whitespace
    return (key, value)

json :: Parser Json
json =
  choice
    "json"
    [ Null <$ null
    , Boolean <$> boolean
    , Number <$> number
    , Array <$> array
    , String <$> Json.Parse.string
    , Object . Map.fromList <$> object
    ]

parser :: Parser Json
parser = json <* many whitespace <* eof

{- |
  Parse a string into a json value
-}
parseJson :: String -> Either ParsingError Json
parseJson = parse parser
