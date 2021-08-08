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

tests :: [Test]
tests =
  [ Decode.specs
  , Parse.specs
  ]

main :: IO ()
main = do
  counts <- runTestTT (test tests)
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
