import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (
  Counts (errors, failures),
  Test (TestCase),
  Testable (test),
  assertEqual,
  runTestTT,
 )

import qualified Decode
import qualified Parse
import qualified Parse2
import qualified ParsingCombinatorsSpecs

tests :: [Test]
tests =
  [ Decode.specs
  , ParsingCombinatorsSpecs.specs
  , Parse2.specs
  ]

main :: IO ()
main = do
  counts <- runTestTT (test tests)
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
