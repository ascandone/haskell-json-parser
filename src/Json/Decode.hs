{- |
  This module is needed to decode a @Json@ type into regular haskell values

@
import Json.Decode as Dec
import Json.Parse (parseJson)

data Person = Person
  { name :: String
  , age :: Maybe Int
  }

json :: Json
json = parseJson "{ \\"name\\" : \\"John Doe\\", \\"age\\":  null  }"

person :: Person
person =
  return Person
    \<*\> field "name" string
    \<*\> field "age" int
    \<*\> optionalField "id" (optional string)

@
-}
module Json.Decode (
  -- * Run decoders
  Decoder,
  decode,
  Error (..),

  -- * Decoders

  -- ** Primitives
  string,
  float,
  bool,
  int,
  null,
  json,

  -- ** Higher order combinators
  list,
  field,
  oneOf,
  nullable,
  optional,
  optionalField,
  at,
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
  Decode a json string value

 >>> decode string (Json.Encode.string "Hello")
 Right "Hello"
-}
string :: Decoder String
string = Decoder $ \x -> case x of
  String value -> Right value
  json -> Left $ failure "string" json

-- | Decode a json boolean value
bool :: Decoder Bool
bool = Decoder $ \x -> case x of
  Boolean value -> Right value
  json -> Left $ failure "boolean" json

-- | Decode a json number value
float :: Decoder Float
float = Decoder $ \x -> case x of
  Number value -> Right value
  json -> Left $ failure "number" json

{- | Decode a json number value that happens to be an integer

 >>> decode int (Json.Encode.number 10.0)
 Right 10

 >>> decode int (Json.Encode.number 10.1)
 Left _
-}
int :: Decoder Int
int = do
  f <- float
  let intPart = (round f) :: Int
  if (realToFrac intPart) == f
    then return intPart
    else fail "int"

-- | Decode a json null value
null :: Decoder ()
null = Decoder $ \x -> case x of
  Null -> Right ()
  json -> Left $ failure "null" json

-- Combinators

{- | Decode a json list

 >>> decode (list float) (Json.Encode.array [Json.Encode.number 1, Json.Encode.number 2])
 Right [1.0,2.0]
-}
list :: Decoder a -> Decoder [a]
list decoder = Decoder $ \x -> case x of
  Array values ->
    foldr reducer (Right []) (zip [0 ..] values)
   where
    reducer (index, x) acc = do
      xValue <- first (Index index) $ decode decoder x
      accValue <- acc
      return (xValue : accValue)
  json -> Left $ failure "array" json

{- | Decode a field of a json object

 >>> decode (field "x" float) (Json.Encode.object [("x", Json.Encode.number 42)])
 Right 42
-}
field :: String -> Decoder a -> Decoder a
field name decoder = Decoder $ \x -> case x of
  Object fieldsMap -> case Map.lookup name fieldsMap of
    Nothing -> Left $ Failure $ "Field \"" ++ name ++ "\" is not found."
    Just json -> first (Field name) $ decode decoder json
  json -> Left $ failure "object" json

-- | Decode nested fields of a json object
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
json = Decoder Right
