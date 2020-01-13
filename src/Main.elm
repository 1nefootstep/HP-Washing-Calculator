module Main exposing (..)

import Html exposing (..)
import Stat.Level as Level
import Jobs
import NaturalHpCalculator

main =
    NaturalHpCalculator.calculateNaturalHp Jobs.Hero (Level.fromInt 200)
    |> Debug.toString
    |> Html.text