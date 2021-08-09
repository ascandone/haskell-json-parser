module ParsingCombinatorsSpecs (specs) where

import Test.HUnit (Test (..), assertEqual)

import ParsingCombinators

specs :: Test
specs =
  TestList
    [ TestLabel "choice backtracks" $
        TestCase $
          let dec = choice "" [string "abc", string "def"]
           in assertEqual "test" (Right "def") (parse dec "def")
    ]
