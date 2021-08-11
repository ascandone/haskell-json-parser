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
  symbol,
  try,
 )
import Prelude hiding (fail, null)

null :: Parser ()
null = void $ symbol "null"

boolean :: Parser Bool
boolean =
  choice
    "boolean"
    [ True <$ symbol "true"
    , False <$ symbol "false"
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
  sign <- optional (symbol "-")
  integerPart <- digits
  fractionalPart <- optional $ do
    symbol "."
    digits

  let n = realToFrac (constructIntegerPart 10 integerPart) + constructFloatingPart (fromMaybe [] fractionalPart)

  case (integerPart, sign) of
    (0 : _ : _, _) -> fail "a number different than zero"
    (_, Just _) -> return (- n)
    (_, Nothing) -> return n

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
    _ -> fail "expected escape char"

-- >>> parse Json.Parse.string "\"\""
-- Right ""

-- >>> parse Json.Parse.string "\"hello\""
-- Right "hello"

-- >>> parse Json.Parse.string "\"he\nllo\""
-- Right "he\nllo"
string :: Parser String
string = between (symbol "\"") (symbol "\"") $
  many $
    try $ do
      ch <- ParsingCombinators.any
      case ch of
        '"' -> fail "expected char"
        '\\' -> escapeChar
        _ -> return ch

-- >>> parse whitespace "x"
-- Left Expected whitespace, got 'x' instead.

-- >>> parse whitespace "\t"
-- Right ()
whitespace :: Parser ()
whitespace =
  void $
    choice
      "whitespace"
      [ symbol " "
      , symbol "\n"
      , symbol "\t"
      , symbol "\r"
      ]

-- >>> parse array "[]"
-- Right []

-- >>> parse array "[null]"
-- Right [null]

-- >>> parse array "[ 1 , 4 ]"
-- Right [1.0,4.0]
array :: Parser [Json]
array = between (symbol "[") (symbol "]") (body `sepBy` separator)
 where
  separator = symbol ","
  body = between (many whitespace) (many whitespace) json

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

-- >>> parse object "{\"key\": _ 42}"
-- Left "At 8:\nExpected a json value, got '_' instead."

object :: Parser [(String, Json)]
object = between (symbol "{") (symbol "}") (keyValue `sepBy` separator)
 where
  separator = symbol ","
  keyValue = do
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
    "a json value"
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

-- >>> parseJson "[2, [20, _]]"
-- Left "At 9:\nExpected a json value, got '_' instead."
parseJson :: String -> Either String Json
parseJson = parse parser
