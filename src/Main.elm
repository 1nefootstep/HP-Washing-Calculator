module Main exposing (..)

import Html exposing (..)
import Stat.Level as Level
import Jobs
import NaturalHp
import NaturalMp

main =
    --NaturalHp.calculateNaturalHp Jobs.Buccaneer (Level.fromInt 200)
    NaturalMp.calculateNaturalMp Jobs.Hero (Level.fromInt 200)
    |> Debug.toString
    |> Html.text