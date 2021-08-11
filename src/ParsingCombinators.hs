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
  symbol,
) where

import Control.Applicative (Alternative (empty, many, some, (<|>)), liftA2, optional)
import Control.Monad (MonadFail (fail), void)
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.Function ((&))
import ParsingCombinators.State (State)
import qualified ParsingCombinators.State as State
import Prelude hiding (any, fail)

data ParsingError = ParsingError
  { index :: Int
  , expected :: String
  , encountered :: String
  }
  deriving (Eq)

instance Show ParsingError where
  show (ParsingError index expected encountered) =
    "At " ++ show index ++ ":\nExpected " ++ expected ++ ", got " ++ encountered ++ " instead."

-- TODO state monad
newtype Parser a = Parser
  { runParser :: State -> Either ParsingError (State, a)
  }

parse :: Parser c -> String -> Either String c
parse parser str =
  case runParser parser (State.make str) of
    Right (_, value) -> Right value
    Left err -> Left (show err)

instance Functor Parser where
  fmap f parser = parser >>= (return . f)

instance Applicative Parser where
  pure = return
  liftA2 binaryFunction parser1 parser2 = do
    x <- parser1
    binaryFunction x <$> parser2

instance Monad Parser where
  return x = Parser $ \state -> Right (state, x)
  parser >>= f = Parser $ \state ->
    runParser parser state >>= (\(state', x) -> runParser (f x) state')

instance MonadFail Parser where
  fail expected = Parser $ \state ->
    Left $
      ParsingError (State.index state) expected $ case State.next state of
        Nothing -> "EOF"
        Just (ch, _) -> show ch

instance Alternative Parser where
  empty = fail "a match"
  (Parser p) <|> (Parser p') = Parser $ \state ->
    case (p state, p' state) of
      (Left _, res) -> res
      (res, _) -> res

-- Primitives

any :: Parser Char
any = Parser $ \state -> case State.next state of
  Nothing -> Left $ ParsingError (State.index state) "any char" "the end of input"
  Just (ch, state') -> Right (state', ch)

eof :: Parser ()
eof = Parser $ \state -> case State.next state of
  Nothing -> Right (state, ())
  Just (ch, state') -> Left $ ParsingError (State.index state') "the end of input" (show ch)

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
symbol = void . string

between :: Parser ignore -> Parser ignore2 -> Parser a -> Parser a
between open close value = open *> value <* close

sepBy, sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy p s = sepBy1 p s <|> return []
sepBy1 p s = do
  first <- p
  rest <- many (s >> p)
  return (first : rest)
