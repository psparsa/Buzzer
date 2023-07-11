module Buzzer where
    buzzer :: Int -> Either String Int
    buzzer n
        | n < 3 = Right n
        | mod n 105 == 0 = Left "FizzBuzzBizz"
        | mod n 35 == 0 = Left "BuzzBizz"
        | mod n 21 == 0 = Left "FizzBizz"
        | mod n 15 == 0 = Left "FizzBuzz"
        | mod n 3 == 0 = Left "Fizz"
        | mod n 5 == 0 = Left "Buzz"
        | mod n 7 == 0 = Left "Bizz"
