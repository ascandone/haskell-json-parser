module ParseSpecs (specs) where

import Json.Parse (parseJson)
import Test.HUnit (Assertion, Test (..), Testable (test), assertEqual)
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
          ] -- TODO escape chars
    , TestCase $
        assertJsEqual "strings" "\"hello\"" (Enc.string "hello")
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
        assertJsEqual "simple objs" "42" (Enc.number 42)
    ]

specs :: Test
specs =
  TestList
    [ TestLabel "primitives" primitives
    , TestLabel "arrays" arrays
    , TestLabel "objects" objects
    ]
