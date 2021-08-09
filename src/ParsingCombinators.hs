module ParsingCombinators (
  Parser,
  parse,
  any,
  eof,
  fail,
  try,
  (<|>),
  many,
  many1,
  char,
  string,
  between,
  sepBy,
) where

import Control.Applicative (Applicative (liftA2))
import Prelude hiding (any, fail)

data ParsingError = ParsingError
  { expected :: String
  , encountered :: String
  }

instance Show ParsingError where
  show (ParsingError expected encountered) =
    "Expected " ++ expected ++ ", got " ++ encountered ++ " instead."

-- TODO state monad
newtype Parser a = Parser
  { runParser :: String -> (String, Either ParsingError a)
  }

parse :: Parser a -> String -> (String, Either ParsingError a)
parse = runParser

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

fail :: String -> String -> Parser a
fail expected encountered = Parser $ \str ->
  ( str
  , Left $ ParsingError expected encountered
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

try :: Parser a -> Parser a
try parser = Parser $ \str ->
  case runParser parser str of
    ok@(_, Right _) -> ok
    (_, left) -> (str, left)

-- TODO Alternative instance
-- TODO change representation using indexes
(<|>) :: Parser a -> Parser a -> Parser a
parser <|> parser' = Parser $ \str ->
  case runParser parser str of
    (str', left@(Left _))
      | str == str' -> runParser parser' str
      | otherwise -> (str', left)
    ok -> ok

-- Combinators

choice :: String -> [Parser a] -> Parser a
choice description = foldr (<|>) (fail description "no match")

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description predicate = do
  ch <- any
  if predicate ch
    then return ch
    else fail description ['"', ch, '"']

char :: Char -> Parser Char
char ch = satisfy ['"', ch, '"'] (== ch)

string :: String -> Parser String
string [] = return []
string (ch : chs) = (:) <$> char ch <*> string chs

between :: Parser ignore -> Parser ignore2 -> Parser a -> Parser a
between open close value = open *> value <* close

many :: Parser a -> Parser [a]
many parser = (try $ (uncurry (:)) <$> many1 parser) <|> return []

many1 :: Parser a -> Parser (a, [a])
many1 parser = (,) <$> parser <*> many parser

sepBy :: ignore -> Parser a
sepBy = undefined
