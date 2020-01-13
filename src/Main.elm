module Main exposing (..)

import Html exposing (..)
import Stat.Level as Level
import Jobs
import NaturalHpCalculator
import NaturalMpCalculator

main =
    --NaturalHpCalculator.calculateNaturalHp Jobs.Buccaneer (Level.fromInt 200)
    NaturalMpCalculator.calculateNaturalMp Jobs.Hero (Level.fromInt 200)
    |> Debug.toString
    |> Html.text