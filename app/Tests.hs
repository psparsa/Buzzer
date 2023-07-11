import           Buzzer      (buzzer)
import qualified System.Exit as Exit
import           Test.HUnit  (Counts (failures), Test (..), assertEqual,
                              runTestTT)

testsBizz :: Test
testsBizz = TestCase (assertEqual "Should return the same value:" 2 (buzzer 2))



buzzerTests :: Test
buzzerTests = TestList [testsBizz]

main :: IO ()
main = do
    result <- runTestTT buzzerTests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

