module Stat.HpStat exposing (..)


type HpStat =
    HpStat Float


zero : HpStat
zero =
    HpStat 0

add : HpStat -> HpStat -> HpStat
add =
    map2 (+)

subtract : HpStat -> HpStat -> HpStat
subtract =
    map2 (-)

multiplyBy : HpStat -> Float -> HpStat
multiplyBy (HpStat a) multiple =
    HpStat (a * multiple)

divideBy : HpStat -> Float -> HpStat
divideBy (HpStat a) divisor =
    HpStat (a / divisor)

fromFloat : Float -> HpStat
fromFloat =
    HpStat

toFloat : HpStat -> Float
toFloat (HpStat float) =
    float

-- map2 takes a function that requires 2 parameters
map2 : (Float -> Float -> Float) -> HpStat -> HpStat -> HpStat
map2 f (HpStat a) (HpStat b) =
    HpStat <| f a b
