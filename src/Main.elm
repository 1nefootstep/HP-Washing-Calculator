module Main exposing (..)

import Html exposing (..)
import JobInMaplestory
import NaturalJobHp

main =
    NaturalJobHp.calculateNaturalHp JobInMaplestory.Warrior 200
    |> Debug.toString
    |> Html.text