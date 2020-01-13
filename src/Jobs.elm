module Jobs exposing (..)
import Stat.Level as Level exposing (Level)
import Stat.HpStat as HpStat exposing (HpStat)
import Stat.MpStat as MpStat exposing (MpStat)
import Maybe


type Job
    = Beginner
    | Hero
    | DarkKnight
    | Paladin
    | Archer
    | Magician
    | Thief
    | Corsair
    | Buccaneer

type alias JobAdvancementLevels =
    { firstJobAdvLevel : Level
    , secondJobAdvLevel : Level
    , thirdJobAdvLevel : Level
    , fourthJobAdvLevel : Level
    }

type alias JobAdvHpGain =
     { firstJobAdvHpGain : HpStat
     , secondJobAdvHpGain : HpStat
     , thirdJobAdvHpGain : HpStat
     , fourthJobAdvHpGain : HpStat
     }

type alias JobAdvMpGain =
      { firstJobAdvMpGain : MpStat
      , secondJobAdvMpGain : MpStat
      , thirdJobAdvMpGain : MpStat
      , fourthJobAdvMpGain : MpStat
      }


getJobAdvLevels : Job -> Maybe JobAdvancementLevels
getJobAdvLevels job =
    let
        standardAdvLevels =
            JobAdvancementLevels
            (Level.fromInt 10)
            (Level.fromInt 30)
            (Level.fromInt 70)
            (Level.fromInt 120)
    in
    case job of
        Beginner ->
            Nothing
        Hero ->
            Just standardAdvLevels
        DarkKnight ->
            Just standardAdvLevels
        Paladin ->
            Just standardAdvLevels
        Archer ->
            Just standardAdvLevels
        Thief ->
            Just standardAdvLevels
        Corsair ->
            Just standardAdvLevels
        Buccaneer ->
            Just standardAdvLevels
        Magician ->
            Just { standardAdvLevels | firstJobAdvLevel = Level.fromInt 8 }

getLevelUpHpGain : Job -> HpStat
getLevelUpHpGain job =
    case job of
        Beginner -> HpStat.fromFloat 14
        Hero -> HpStat.fromFloat 66
        DarkKnight -> HpStat.fromFloat 66
        Paladin -> HpStat.fromFloat 66
        Archer -> HpStat.fromFloat 22
        Thief -> HpStat.fromFloat 22
        Corsair -> HpStat.fromFloat 22
        Buccaneer -> HpStat.fromFloat 55
        Magician -> HpStat.fromFloat 12

getLevelUpMpGain : Job -> MpStat
getLevelUpMpGain job =
    case job of
        Beginner -> MpStat.fromFloat 11
        Hero -> MpStat.fromFloat 5
        DarkKnight -> MpStat.fromFloat 5
        Paladin -> MpStat.fromFloat 5
        Archer -> MpStat.fromFloat 15
        Thief -> MpStat.fromFloat 15
        Corsair -> MpStat.fromFloat 20.5
        Buccaneer -> MpStat.fromFloat 20.5
        Magician -> MpStat.fromFloat 43

getJobAdvHpGain : Job -> Maybe JobAdvHpGain
getJobAdvHpGain job =
    let
        standardJobAdvHpGain =
            JobAdvHpGain
                (HpStat.fromFloat 162)
                (HpStat.fromFloat 325)
                (HpStat.fromFloat 625)
                (HpStat.fromFloat 925)
    in
    case job of
        Beginner ->
            Nothing
        Hero ->
            Just (JobAdvHpGain
                (HpStat.fromFloat 225)
                (HpStat.fromFloat 325)
                (HpStat.fromFloat 1025)
                (HpStat.fromFloat 1825)
            )
        DarkKnight ->
            Just (JobAdvHpGain
                (HpStat.fromFloat 225)
                (HpStat.fromFloat 325)
                (HpStat.fromFloat 1025)
                (HpStat.fromFloat 1825)
            )
        Paladin ->
            Just (JobAdvHpGain
                (HpStat.fromFloat 225)
                (HpStat.fromFloat 325)
                (HpStat.fromFloat 1025)
                (HpStat.fromFloat 1825)
            )
        Archer ->
            Just standardJobAdvHpGain
        Thief ->
            Just standardJobAdvHpGain
        Corsair ->
            Just standardJobAdvHpGain
        Buccaneer ->
            Just standardJobAdvHpGain
        Magician ->
            Just (JobAdvHpGain
                (HpStat.fromFloat 0)
                (HpStat.fromFloat 0)
                (HpStat.fromFloat 0)
                (HpStat.fromFloat 0)
            )

getJobAdvMpGain : Job -> Maybe JobAdvMpGain
getJobAdvMpGain job =
    let
        standard175MpJobAdvGain =
            JobAdvMpGain
                (MpStat.fromFloat 0)
                (MpStat.fromFloat 175)
                (MpStat.fromFloat 175)
                (MpStat.fromFloat 175)
    in
    case job of
        Beginner ->
            Nothing
        Hero ->
            Just (JobAdvMpGain
                (MpStat.fromFloat 0)
                (MpStat.fromFloat 0)
                (MpStat.fromFloat 0)
                (MpStat.fromFloat 0)
            )
        DarkKnight ->
            Just (JobAdvMpGain
                (MpStat.fromFloat 0)
                (MpStat.fromFloat 125)
                (MpStat.fromFloat 125)
                (MpStat.fromFloat 125)
            )
        Paladin ->
            Just (JobAdvMpGain
                (MpStat.fromFloat 0)
                (MpStat.fromFloat 125)
                (MpStat.fromFloat 125)
                (MpStat.fromFloat 125)
            )
        Archer ->
            Just standard175MpJobAdvGain
        Thief ->
            Just standard175MpJobAdvGain
        Corsair ->
            Just standard175MpJobAdvGain
        Buccaneer ->
            Just standard175MpJobAdvGain
        Magician ->
            Just (JobAdvMpGain
                (MpStat.fromFloat 125)
                (MpStat.fromFloat 475)
                (MpStat.fromFloat 475)
                (MpStat.fromFloat 475)
            )