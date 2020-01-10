module Level exposing (..)

type alias Level =
    Int

restrictLevel : Level -> Level
restrictLevel level =
    clamp 1 200 level
