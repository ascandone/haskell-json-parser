module Json.Decode (
  Decoder,
  Error (..),
  decode,
  string,
  number,
  boolean,
  array,
  field,
  oneOf,
  nullable,
  null,
  optional,
  optionalField,
  at,
  json,
) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (ap, forM_)
import Control.Monad.Fail (MonadFail (fail))
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Json.Internal (Json (..))
import Prelude hiding (null)

data Error
  = Failure String
  | Index Int Error
  | Field String Error
  | OneOf [Error]
  deriving (Show, Eq)

newtype Decoder a = Decoder
  { runDecoder :: Json -> Either Error a
  }

instance Functor Decoder where
  fmap f decoder = decoder >>= (return . f)

instance Applicative Decoder where
  pure = Decoder . const . Right
  liftA2 binaryFunction decoder1 decoder2 = do
    x <- decoder1
    binaryFunction x <$> decoder2

instance Monad Decoder where
  return = pure
  decoder >>= f = Decoder $ \json -> do
    decode decoder json >>= \value -> decode (f value) json

instance MonadFail Decoder where
  fail reason = Decoder $ \json -> Left $ Failure reason

decode :: Decoder a -> Json -> Either Error a
decode = runDecoder

tag :: Json -> String
tag (Number _) = "number"
tag (Boolean _) = "boolean"
tag (String _) = "string"
tag (Array _) = "array"
tag (Object _) = "object"
tag Null = "null"

failure :: String -> Json -> Error
failure expected got =
  Failure $ "Expected: \"" ++ expected ++ "\". Got \"" ++ tag got ++ "\" instead."

-- Primitives

{- |
  Decode a string

 >>> decode string (Json.Encode.string "Hello")
 Right "Hello"
-}
string :: Decoder String
string = Decoder $ \x -> case x of
  String value -> Right value
  json -> Left $ failure "string" json

boolean :: Decoder Bool
boolean = Decoder $ \x -> case x of
  Boolean value -> Right value
  json -> Left $ failure "boolean" json

number :: Decoder Float
number = Decoder $ \x -> case x of
  Number value -> Right value
  json -> Left $ failure "number" json

null :: Decoder ()
null = Decoder $ \x -> case x of
  Null -> Right ()
  json -> Left $ failure "null" json

-- Combinators

array :: Decoder a -> Decoder [a]
array decoder = Decoder $ \x -> case x of
  Array values ->
    foldr reducer (Right []) (zip [0 ..] values)
   where
    reducer (index, x) acc = do
      xValue <- first (Index index) $ decode decoder x
      accValue <- acc
      return (xValue : accValue)
  json -> Left $ failure "array" json

field :: String -> Decoder a -> Decoder a
field name decoder = Decoder $ \x -> case x of
  Object fieldsMap -> case Map.lookup name fieldsMap of
    Nothing -> Left $ Failure $ "Field \"" ++ name ++ "\" is not found."
    Just json -> first (Field name) $ decode decoder json
  json -> Left $ failure "object" json

at :: [String] -> Decoder a -> Decoder a
at [] _ = fail "Expected fields"
at [name] decoder = field name decoder
at (name : names) decoder = field name (at names decoder)

oneOf :: [Decoder a] -> Decoder a
oneOf decoders = Decoder $ \json ->
  let recur [] errors = Left $ reverse $ errors
      recur (decoder : rest) errors =
        case decode decoder json of
          Right value -> Right value
          Left error -> recur rest (error : errors)
   in first OneOf $ recur decoders []

optional :: Decoder a -> Decoder (Maybe a)
optional decoder =
  oneOf
    [ Just <$> decoder
    , return Nothing
    ]

optionalField :: String -> Decoder a -> Decoder (Maybe a)
optionalField name decoder = field name (optional decoder)

nullable :: Decoder a -> Decoder (Maybe a)
nullable decoder =
  oneOf
    [ Nothing <$ null
    , Just <$> decoder
    ]

json :: Decoder Json
json = Decoder $ Right
