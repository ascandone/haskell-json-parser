import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (
  Counts (errors, failures),
  Test (TestCase),
  Testable (test),
  assertEqual,
  runTestTT,
 )

import qualified DecodeSpecs
import qualified Parse2Specs
import qualified ParseSpecs
import qualified ParsingCombinatorsSpecs

tests :: [Test]
tests =
  [ DecodeSpecs.specs
  , ParsingCombinatorsSpecs.specs
  , Parse2Specs.specs
  ]

main :: IO ()
main = do
  counts <- runTestTT (test tests)
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
