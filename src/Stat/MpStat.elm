module Stat.MpStat exposing (..)


type MpStat =
    MpStat Float


zero : MpStat
zero =
    MpStat 0

add : MpStat -> MpStat -> MpStat
add =
    map2 (+)

subtract : MpStat -> MpStat -> MpStat
subtract =
    map2 (-)

multiplyBy : MpStat -> Float -> MpStat
multiplyBy (MpStat a) multiple =
    MpStat (a * multiple)

divideBy : MpStat -> Float -> MpStat
divideBy (MpStat a) divisor =
    MpStat (a / divisor)

fromFloat : Float -> MpStat
fromFloat =
    MpStat

toFloat : MpStat -> Float
toFloat (MpStat float) =
    float

-- map2 takes a function that requires 2 parameters
map2 : (Float -> Float -> Float) -> MpStat -> MpStat -> MpStat
map2 f (MpStat a) (MpStat b) =
    MpStat <| f a b
