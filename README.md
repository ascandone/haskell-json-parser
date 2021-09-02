# json-parser

Lightweight library for json parsing, encoding and decoding with no external dependencies

[Api docs](https://ascandone.github.io/haskell-json-parser/)

> **warning** this library is a minimal implementation built from scratch of a json parser and is not meant for production use. It follows the complete [json specs](https://www.json.org/json-en.html) and has some tests, but there might be some bugs, it is not optimized for large json strings, and most of docs are missing yet

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
  , age :: Int
  , id :: Maybe String
  }

personDecoder :: Decoder Person
personDecoder = do
  name <- field "name" string
  age <- field "age" int
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
    <*> field "age" int
    <*> optionalField "id" (optional string)
```
