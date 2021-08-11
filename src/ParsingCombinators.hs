module ParsingCombinators (
  Parser,
  parse,
  any,
  eof,
  fail,
  char,
  string,
  between,
  sepBy,
  choice,
  ParsingError,
  digit,
  satisfy,
  hexDigit,
  try,
  symbol,
) where

import Control.Applicative (Alternative (empty, many, some, (<|>)), liftA2, optional)
import Control.Monad (void)
import Data.Char (toLower)
import Prelude hiding (any, fail)

data ParsingError = ParsingError
  { expected :: String
  , encountered :: String
  }
  deriving (Eq)

instance Show ParsingError where
  show (ParsingError expected encountered) =
    "Expected " ++ expected ++ ", got " ++ encountered ++ " instead."

-- TODO state monad
newtype Parser a = Parser
  { runParser :: String -> (String, Either ParsingError a)
  }

parse :: Parser a -> String -> Either ParsingError a
parse parser str = let (_, result) = runParser parser str in result

instance Functor Parser where
  fmap f parser = parser >>= (return . f)

instance Applicative Parser where
  pure = return
  liftA2 binaryFunction parser1 parser2 = do
    x <- parser1
    binaryFunction x <$> parser2

instance Monad Parser where
  return x = Parser $ \str -> (str, Right x)
  parser >>= f = Parser $ \str ->
    case runParser parser str of
      (str', Right x) -> runParser (f x) str'
      (str', Left e) -> (str', Left e)

instance Alternative Parser where
  empty = fail "a match"
  parser <|> parser' = Parser $ \str ->
    case runParser parser str of
      (str', left@(Left _))
        | str == str' -> runParser parser' str
        | otherwise -> (str', left)
      ok -> ok

try :: Parser a -> Parser a
try parser = Parser $ \str ->
  case runParser parser str of
    (_, left@(Left _)) -> (str, left)
    ok -> ok

fail :: String -> Parser a
fail expected = Parser $ \str ->
  ( str
  , Left $
      ParsingError expected $ case str of
        [] -> "EOF"
        hd : _ -> show hd
  )

-- Primitives
any :: Parser Char
any = Parser $ \str -> case str of
  [] -> ([], Left $ ParsingError "any char" "the end of input")
  c : cs -> (cs, Right c)

eof :: Parser ()
eof = Parser $ \str -> case str of
  [] -> ([], Right ())
  c : _ -> (str, Left $ ParsingError "the end of input" ['"', c, '"'])

-- Combinators

choice :: String -> [Parser a] -> Parser a
choice description = foldr (<|>) (fail description)

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description predicate = do
  ch <- any
  if predicate ch
    then return ch
    else fail description

char :: Char -> Parser Char
char ch = satisfy ['"', ch, '"'] (== ch)

digit :: Parser Int
digit =
  any >>= \ch -> case ch of
    '0' -> return 0
    '1' -> return 1
    '2' -> return 2
    '3' -> return 3
    '4' -> return 4
    '5' -> return 5
    '6' -> return 6
    '7' -> return 7
    '8' -> return 8
    '9' -> return 9
    _ -> fail "a digit"

hexDigit :: Parser Int
hexDigit =
  any >>= \ch -> case toLower ch of
    '0' -> return 0
    '1' -> return 1
    '2' -> return 2
    '3' -> return 3
    '4' -> return 4
    '5' -> return 5
    '6' -> return 6
    '7' -> return 7
    '8' -> return 8
    '9' -> return 9
    'a' -> return 10
    'b' -> return 11
    'c' -> return 12
    'd' -> return 13
    'e' -> return 14
    'f' -> return 15
    _ -> fail "an hex digit"

string :: String -> Parser String
string [] = return []
string (ch : chs) = (:) <$> char ch <*> string chs

symbol :: String -> Parser ()
symbol = void . try . string

between :: Parser ignore -> Parser ignore2 -> Parser a -> Parser a
between open close value = open *> value <* close

sepBy, sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy p s = sepBy1 p s <|> return []
sepBy1 p s = do
  first <- p
  rest <- many (s >> p)
  return (first : rest)
