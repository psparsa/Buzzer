import           Buzzer      (buzzer)
import qualified System.Exit as Exit
import           Test.HUnit  (Counts (failures), Test (..), assertEqual,
                              assertFailure, runTestTT)

wrongTypeMsg :: String
wrongTypeMsg = "Function returned the wrong type!"

divTestMsg :: String -> String
divTestMsg n = "Tests the case where the input number is divisible by " ++ n

testLessThanThree :: Test
testLessThanThree = TestCase $ do
    let result = buzzer 2
    case result of
        Right number -> assertEqual "Tests with a number less than 3" 2 number
        Left _       -> assertFailure wrongTypeMsg

testThree :: Test
testThree = TestCase $ do
    let result = buzzer 3
    case result of
        Right _  -> assertFailure wrongTypeMsg
        Left msg -> assertEqual (divTestMsg "3") "Fizz" msg


testFive :: Test
testFive = TestCase $ do
    let result = buzzer 5
    case result of
        Right _  -> assertFailure wrongTypeMsg
        Left msg -> assertEqual (divTestMsg "5") "Buzz" msg


testSeven :: Test
testSeven = TestCase $ do
    let result = buzzer 7
    case result of
        Right _  -> assertFailure wrongTypeMsg
        Left msg -> assertEqual (divTestMsg "7") "Bizz" msg


testFifteen :: Test
testFifteen = TestCase $ do
    let result = buzzer 15
    case result of
        Right _  -> assertFailure wrongTypeMsg
        Left msg -> assertEqual (divTestMsg "3 and 5") "FizzBuzz" msg

testTwentyOne :: Test
testTwentyOne = TestCase $ do
    let result = buzzer 21
    case result of
        Right _  -> assertFailure wrongTypeMsg
        Left msg -> assertEqual (divTestMsg "3 and 7") "FizzBizz" msg

testThirtyFive :: Test
testThirtyFive = TestCase $ do
    let result = buzzer 35
    case result of
        Right _  -> assertFailure wrongTypeMsg
        Left msg -> assertEqual (divTestMsg "5 and 7") "BuzzBizz" msg

testOneHundredFiveThirtyFive :: Test
testOneHundredFiveThirtyFive = TestCase $ do
    let result = buzzer 105
    case result of
        Right _  -> assertFailure wrongTypeMsg
        Left msg -> assertEqual (divTestMsg "3, 5 and 7") "FizzBuzzBizz" msg

testANumberWithDigitThree :: Test
testANumberWithDigitThree = TestCase $ do
    let result = buzzer 111333
    case result of
        Right _     -> assertFailure wrongTypeMsg
        Left msg -> assertEqual "Tests a number which has at least a 3" "Fizz" msg

buzzerTests :: Test
buzzerTests = TestList [
    testLessThanThree,
    testThree,
    testFive,
    testSeven,
    testFifteen,
    testTwentyOne,
    testThirtyFive,
    testOneHundredFiveThirtyFive,
    testANumberWithDigitThree
    ]

main :: IO ()
main = do
    result <- runTestTT buzzerTests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

