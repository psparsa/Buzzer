module Buzzer (buzzer) where
    containsThree :: Int -> Bool
    containsThree n = elem '3' (show n)

    isDivisible :: Integral a => a -> a -> Bool
    isDivisible number divisor = mod number divisor == 0

    isPrime :: Int -> Bool
    isPrime n
        | n < 2 = False
        | otherwise = null [x | x <- [2..sqrtN], (mod n x) == 0]
        where sqrtN = floor (sqrt (fromIntegral n))

    buzzer :: Int -> Either String Int
    buzzer n
        | n < 3 = Right n
        | isDivisible n 105 = Left "FizzBuzzBizz"
        | isDivisible n 35 = Left "BuzzBizz"
        | isDivisible n 21 = Left "FizzBizz"
        | isDivisible n 15 = Left "FizzBuzz"
        | isDivisible n 3 = Left "Fizz"
        | isDivisible n 5 = Left "Buzz"
        | isDivisible n 7 = Left "Bizz"
        | containsThree n = Left "Fizz"
        | isPrime n = Left "Pizz"

