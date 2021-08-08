module Json.Parse (parseJson) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void (Void)
import Json.Internal (Json (..))
import Text.Megaparsec (
  MonadParsec (eof, try),
  Parsec,
  between,
  choice,
  many,
  manyTill,
  oneOf,
  parse,
  sepBy,
  skipMany,
 )
import Text.Megaparsec.Char (char, letterChar, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)
import Prelude hiding (null)

type Parser = Parsec Void String

-- Utils
spaceConsumer :: Parser ()
spaceConsumer = skipMany (char ' ')

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- Primitives

null :: Parser ()
null = void (symbol "null")

bool :: Parser Bool
bool =
  choice
    [ True <$ symbol "true"
    , False <$ symbol "false"
    ]

number :: Parser Float
number =
  let value = try L.float <|> try L.decimal
   in choice
        [ fmap negate (char '-' >> value)
        , value
        ]

array :: Parser [Json]
array =
  between (symbol "[") (symbol "]") (json `sepBy` symbol ",")

-- TODO letterChar
string :: Parser String
string = do
  symbol "\""
  letterChar `manyTill` symbol "\""

object :: Parser (Map String Json)
object =
  let keyValue :: Parser (String, Json)
      keyValue = do
        key <- Json.Parse.string
        symbol ":"
        value <- json
        return (key, value)
   in Map.fromList
        <$> between (symbol "{") (symbol "}") (keyValue `sepBy` symbol ",")

json :: Parser Json
json =
  choice
    [ Null <$ null
    , Boolean <$> bool
    , Number <$> number
    , String <$> Json.Parse.string
    , Array <$> array
    , Object <$> object
    ]

parseJson :: String -> Either (ParseErrorBundle String Void) Json
parseJson = parse (json <* eof) ""
