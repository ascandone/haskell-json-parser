module ParseSpecs (specs) where

import Json.Parse (parseJson)
import Test.HUnit (Assertion, AssertionPredicable (assertionPredicate), Test (..), Testable (test), assertEqual, assertFailure)
import Test.HUnit.Lang (assertEqual)

import Json (Json)
import qualified Json.Encode as Enc

assertJsEqual :: String -> String -> Json -> Assertion
assertJsEqual label stringJson exp =
  assertEqual label (parseJson stringJson) (Right exp)

primitives :: Test
primitives =
  TestList
    [ TestCase $
        assertJsEqual "null" "null" (Enc.null)
    , TestLabel "booleans" $
        TestList
          [ TestCase $
              assertJsEqual "true" "true" (Enc.boolean True)
          , TestCase $
              assertJsEqual "false" "false" (Enc.boolean False)
          ]
    , TestLabel "numbers" $
        TestList
          [ TestCase $
              assertJsEqual "zero" "0" (Enc.number 0.0)
          , TestCase $
              assertJsEqual "integer" "10" (Enc.number 10.0)
          , TestCase $
              assertJsEqual "negative integer" "-10" (Enc.number (-10.0))
          , TestCase $
              assertJsEqual "float" "1.1" (Enc.number 1.1)
          ]
    , TestLabel "strings" $
        TestList
          [ TestCase $
              assertJsEqual "simple" "\"hello\"" (Enc.string "hello")
          , TestCase $
              assertJsEqual "newline" "\"<\\n>\"" (Enc.string "<\n>")
          , TestCase $
              assertJsEqual "backslash" "\"<\\\">\"" (Enc.string "<\">")
          , TestCase $
              assertJsEqual "unicode" "\"<\\u26A1>\"" (Enc.string "<⚡>")
          ]
    ]

arrays :: Test
arrays =
  TestList
    [ TestCase $
        assertJsEqual
          "empty arr"
          "[]"
          (Enc.array [])
    , TestCase $
        assertJsEqual
          "arr of nums"
          "[1, 2, 3]"
          (Enc.array [Enc.number 1, Enc.number 2, Enc.number 3])
    , TestCase $
        assertJsEqual
          "arr without commas"
          "[1,2,3]"
          (Enc.array [Enc.number 1, Enc.number 2, Enc.number 3])
    , TestLabel "arr with whitespaces" $
        let value = Enc.array [Enc.number 1, Enc.number 2]
         in TestList
              [ TestCase $
                  assertJsEqual
                    "arr with commas"
                    "[1,   2]"
                    value
              , TestCase $
                  assertJsEqual
                    "arr with commas"
                    "[1  \r  \n , \t   2]"
                    value
              , TestCase $
                  case parseJson "[1, 2, ???]" of
                    Left _ -> return ()
                    Right _ -> assertFailure "invalid arr"
              , TestCase $
                  case parseJson "[1, 2 ???]" of
                    Left _ -> return ()
                    Right _ -> assertFailure "invalid arr"
              ]
    , TestCase $
        assertJsEqual
          "heterogeneous arr"
          "[1, \"x\", null]"
          (Enc.array [Enc.number 1, Enc.string "x", Enc.null])
    ]

objects :: Test
objects =
  TestList
    [ TestCase $
        assertJsEqual "empty obj" "{}" (Enc.object [])
    , TestCase $
        assertJsEqual "simple obj" "{\"x\":42}" (Enc.object [("x", Enc.number 42)])
    , TestCase $
        assertJsEqual
          "two keys"
          "{\"x\":0,\"y\":1}"
          ( Enc.object [("x", Enc.number 0), ("y", Enc.number 1)]
          )
    , TestCase $
        assertJsEqual
          "two keys white whitespace"
          "{  \"x\"  \t :  0  ,\n \"y\":1}"
          ( Enc.object [("x", Enc.number 0), ("y", Enc.number 1)]
          )
    ]

specs :: Test
specs =
  TestList
    [ TestLabel "primitives" primitives
    , TestLabel "arrays" arrays
    , TestLabel "objects" objects
    ]
