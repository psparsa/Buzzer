module Main where
import           Buzzer (buzzer)

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
        line <- getLine
        let buzzerResult = buzzer . readInt $ line
        case buzzerResult of
                Right num -> do
                        putStrLn "Return Type Was Number"
                        putStr "Result: "
                        print num
                Left str -> do
                        putStrLn "Return Type Was String"
                        putStr "Result: "
                        print str
