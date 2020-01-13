module Stat.Level exposing (..)


type Level =
    Level Int

-- use this to construct a Level instead of the default
-- to ensure that Level is within 1 to 200 inclusive.
fromInt : Int -> Level
fromInt int =
    if int < 1 then
        Level 1
    else if int > 200 then
        Level 200
    else
        Level int

zero : Level
zero =
    Level 0

one : Level
one =
    Level 1

add : Level -> Level -> Level
add =
    map2 (+)

subtract : Level -> Level -> Level
subtract =
    map2 (-)

multiplyBy : Level -> Int -> Level
multiplyBy (Level a) multiple =
    Level (a * multiple)

divideBy : Level -> Int -> Level
divideBy (Level a) divisor =
    Level (a // divisor)

lessThan : Level -> Level -> Bool
lessThan (Level a) (Level b) =
    a < b

equalTo : Level -> Level -> Bool
equalTo (Level a) (Level b) =
    a == b

greaterThan : Level -> Level -> Bool
greaterThan (Level a) (Level b) =
    a > b

toInt : Level -> Int
toInt (Level int) =
    int

-- map takes a function that requires 1 parameter
map : (Int -> Int) -> Level -> Level
map f (Level a) =
    Level <| f a

-- map2 takes a function that requires 2 parameters
map2 : (Int -> Int -> Int) -> Level -> Level -> Level
map2 f (Level a) (Level b) =
    Level <| f a b
