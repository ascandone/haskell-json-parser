module Parse (specs) where

import Json.Parse (parseJson)
import Test.HUnit (Assertion, Test (..), Testable (test), assertEqual)
import Test.HUnit.Lang (assertEqual)

import Json (Json)
import qualified Json.Encode as Enc

assertJsEqual :: String -> String -> Json -> Assertion
assertJsEqual label stringJson exp =
  assertEqual label (parseJson stringJson) (Right exp)

jsNull :: Test
jsNull =
  TestCase $
    assertJsEqual "null" "null" (Enc.null)

jsNumber :: Test
jsNumber =
  TestCase $
    assertJsEqual "simple objs" "42" (Enc.number 42)

-- TODO escape chars
jsStr :: Test
jsStr =
  TestCase $
    assertJsEqual "simple objs" "\"hello\"" (Enc.string "hello")

jsBool :: Test
jsBool =
  test
    [ TestCase $
        assertJsEqual "true" "true" (Enc.boolean True)
    , TestCase $
        assertJsEqual "false" "false" (Enc.boolean False)
    ]

jsArr :: Test
jsArr =
  TestCase $
    assertJsEqual
      "arr of nums"
      "[1, 2, 3]"
      ( Enc.array [Enc.number 1, Enc.number 2, Enc.number 3]
      )

jsObj :: Test
jsObj =
  TestCase $
    assertJsEqual
      "simple object"
      "{\"x\": 42}"
      ( Enc.object [("x", Enc.number 42)]
      )

specs :: Test
specs =
  test
    [ jsNumber
    , jsStr
    , jsBool
    , jsArr
    , jsNull
    ]
