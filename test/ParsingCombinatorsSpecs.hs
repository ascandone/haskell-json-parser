module ParsingCombinatorsSpecs (specs) where

import Test.HUnit (Test (..), assertEqual)

import Control.Applicative
import ParsingCombinators (parse, string, symbol)

specs :: Test
specs =
    TestList
        [ TestLabel "optional works as expected" $
            let dec = do
                    k <- optional (symbol "k")
                    xy <- string "xy"
                    return (k, xy)
             in TestList
                    [ TestCase $
                        assertEqual "no value" (Right (Nothing, "xy")) $ parse dec "xy"
                    , TestCase $
                        assertEqual "value" (Right (Just (), "xy")) $ parse dec "kxy"
                    ]
        ]
