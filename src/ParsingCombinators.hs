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
import Control.Monad (MonadFail (fail), void)
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.Function ((&))
import Prelude hiding (any, fail)

data ParsingError = ParsingError
  { expected :: String
  , encountered :: String
  }
  deriving (Eq)

instance Show ParsingError where
  show (ParsingError expected encountered) =
    "Expected " ++ expected ++ ", got " ++ encountered ++ " instead."

data State = State {index :: Int, current :: String}

make :: String -> State
make str = State 0 str

next :: State -> Maybe (Char, State)
next (State _ "") = Nothing
next (State str (ch : chs)) = Just (ch, State str chs)

instance Eq State where
  (State i _) == (State i' _) = i == i

-- TODO state monad
newtype Parser a = Parser
  { runParser :: State -> (State, Either ParsingError a)
  }

parse :: Parser c -> String -> Either String c
parse parser str =
  let (State i _, result) = runParser parser (make str)
   in result & first (\err -> "At " ++ show i ++ ":\n" ++ show err)

instance Functor Parser where
  fmap f parser = parser >>= (return . f)

instance Applicative Parser where
  pure = return
  liftA2 binaryFunction parser1 parser2 = do
    x <- parser1
    binaryFunction x <$> parser2

instance Monad Parser where
  return x = Parser $ \state -> (state, Right x)
  parser >>= f = Parser $ \state ->
    case runParser parser state of
      (state', Right x) -> runParser (f x) state'
      (state', Left e) -> (state', Left e)

instance MonadFail Parser where
  fail expected = Parser $ \state ->
    ( state
    , Left $
        ParsingError expected $ case next state of
          Nothing -> "EOF"
          Just (ch, _) -> show ch
    )

instance Alternative Parser where
  empty = fail "a match"
  parser <|> parser' = Parser $ \state ->
    case runParser parser state of
      (state', left@(Left _))
        | state == state' -> runParser parser' state
        | otherwise -> (state', left)
      ok -> ok

-- Primitives

try :: Parser a -> Parser a
try parser = Parser $ \state ->
  case runParser parser state of
    (_, left@(Left _)) -> (state, left)
    ok -> ok

any :: Parser Char
any = Parser $ \state -> case next state of
  Nothing -> (state, Left $ ParsingError "any char" "the end of input")
  Just (ch, state') -> (state', Right ch)

eof :: Parser ()
eof = Parser $ \state -> case next state of
  Nothing -> (state, Right ())
  Just (ch, state') -> (state', Left $ ParsingError "the end of input" (show ch))

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
