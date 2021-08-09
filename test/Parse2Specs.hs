module Parse2Specs (specs) where

import Json.Parse2 (parseJson)
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
    , -- TODO escape chars
      TestLabel "strings" $
        TestList
          [ TestCase $
              assertJsEqual "strings" "\"hello\"" (Enc.string "hello")
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
              -- , TestCase $
              --     assertJsEqual
              --       "heterogeneous arr"
              --       "[1, \"x\", null]"
              --       (Enc.array [Enc.number 1, Enc.string "x", Enc.null])
    ]

-- objects :: Test
-- objects =
--   TestList
--     [ TestCase $
--         assertJsEqual "simple objs" "42" (Enc.number 42)
--     ]

specs :: Test
specs =
  TestList
    [ TestLabel "primitives" primitives
    , TestLabel "arrays" arrays
    -- , TestLabel "objects" objects
    ]
