{-# LANGUAGE NamedFieldPuns #-}

module DecodeSpecs (specs) where

import Common (Person (Person))
import qualified Data.Map as Map
import Json (Json)
import Json.Decode (Decoder)
import qualified Json.Decode as Dec
import qualified Json.Encode as Enc
import Test.HUnit (Test (..), Testable (test), assertEqual)

personJson :: Json
personJson =
  Enc.object
    [ ("name", Enc.string "John Doe")
    , ("age", Enc.number 42.0)
    , ("isDeveloper", Enc.boolean True)
    ]

personDecoder :: Decoder Person
personDecoder =
  return Person
    <*> Dec.field "name" Dec.string
    <*> Dec.field "age" Dec.int
    <*> Dec.optionalField "isDeveloper" Dec.bool

firstTest :: Test
firstTest =
  TestCase
    ( assertEqual
        "testEncode"
        (Dec.decode personDecoder personJson)
        (Right $ Person "John Doe" 42 (Just True))
    )

specs :: Test
specs = test [firstTest]
