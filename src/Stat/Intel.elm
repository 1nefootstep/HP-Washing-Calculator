module Stat.Intel exposing (..)


type Intel =
    Intel Int


zero : Intel
zero =
    Intel 0

add : Intel -> Intel -> Intel
add =
    map2 (+)

subtract : Intel -> Intel -> Intel
subtract =
    map2 (-)

multiplyBy : Intel -> Int -> Intel
multiplyBy (Intel a) multiple =
    Intel (a * multiple)

divideBy : Intel -> Int -> Intel
divideBy (Intel a) divisor =
    Intel (a // divisor)

lessThan : Intel -> Intel -> Bool
lessThan (Intel a) (Intel b) =
    a < b

equalTo : Intel -> Intel -> Bool
equalTo (Intel a) (Intel b) =
    a == b

fromInt : Int -> Intel
fromInt =
    Intel

toInt : Intel -> Int
toInt (Intel int) =
    int

-- map2 takes a function that requires 2 parameters
map2 : (Int -> Int -> Int) -> Intel -> Intel -> Intel
map2 f (Intel a) (Intel b) =
    Intel <| f a b
