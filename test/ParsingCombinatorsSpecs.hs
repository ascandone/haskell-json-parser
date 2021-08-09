module ParsingCombinatorsSpecs (specs) where

import Test.HUnit (Test (..), assertEqual)

import Control.Applicative
import ParsingCombinators

specs :: Test
specs =
  TestList
    [ TestCase $
        let dec = choice "" [string "abc", string "def"]
         in assertEqual "choice backtracks" (Right "def") (parse dec "def")
    , TestLabel "optional works as expected" $
        let dec = do
              k <- optional (char 'k')
              xy <- string "xy"
              return (k, xy)
         in TestList
              [ TestCase $
                  assertEqual "no value" (Right (Nothing, "xy")) $ parse dec "xy"
              , TestCase $
                  assertEqual "value" (Right (Just 'k', "xy")) $ parse dec "kxy"
              ]
    ]
