module Main where
import           Buzzer (buzzer)

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
        line <- getLine
        print . buzzer . readInt $ line
