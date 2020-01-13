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

multiplyBy : Float -> MpStat -> MpStat
multiplyBy multiple=
    map <| (*) multiple

divideBy : Float -> MpStat -> MpStat
divideBy divisor =
    map <| (\level -> level / divisor)

fromFloat : Float -> MpStat
fromFloat =
    MpStat

toFloat : MpStat -> Float
toFloat (MpStat float) =
    float

-- map takes a function that requires 1 parameter
map : (Float -> Float) -> MpStat -> MpStat
map f (MpStat a) =
    MpStat <| f a

-- map2 takes a function that requires 2 parameters
map2 : (Float -> Float -> Float) -> MpStat -> MpStat -> MpStat
map2 f (MpStat a) (MpStat b) =
    MpStat <| f a b
