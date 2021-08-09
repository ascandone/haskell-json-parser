# json-parser

Json decoders and encoders.

> **warning** this library is a minimal implementation of a json parser and is not meant for production use.

This module has 3 modules: `Json`, `Json.Parse`, `Json.Decode` and `Json.Encode`

`Json` exposes the opaque datatype `Json`, the `AST` of a json object.

`Json.Parse` exposes `parseJson`, a function that parses (if possible) a string into a `Json` value.

`Json.Encode` exposes combinators to transform an haskell value into a `Json` value

```hs
personJson :: Json
personJson = object
  [ ("name", string "John Doe")
  , ("age", number 24)
  , ("id", null)
  ]
```

`Json.Decode` exposes combinators to transform a `Json` value into regular haskell values


It supports both monadic instance

```hs
{-# LANGUAGE NamedFieldPuns #-}

data Person = Person
  { name :: String
  , age :: Float
  , id :: Maybe String
  }

personDecoder :: Decoder Person
personDecoder = do
  name <- field "name" string
  age <- field "age" number
  id <- field "id" (optional string)
  return Person{name, age, id}

person :: Either Error Person
person = decode personDecoder personJson
```

or applicative instance:

```hs
personDecoder :: Decoder Person
personDecoder =
  return Person
    <*> field "name" string
    <*> field "age" number
    <*> optionalField "id" (optional string)
```
