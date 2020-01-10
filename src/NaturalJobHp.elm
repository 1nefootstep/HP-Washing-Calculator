module NaturalJobHp exposing (calculateNaturalHp)

import Level
import JobInMaplestory exposing (Job(..))


type alias JobInfo =
    { firstJobAdvHpGain : Int
    , secondJobAdvHpGain : Int
    , thirdJobAdvHpGain : Int
    , fourthJobAdvHpGain : Int
    , naturalHpGain : Int
    , firstJobAdvLevel : Int
    , secondJobAdvLevel : Int
    , thirdJobAdvLevel : Int
    , fourthJobAdvLevel : Int
    }

calculateNaturalHp : Job -> Level.Level -> Int
calculateNaturalHp job level =
    let
        warriorJobInfo =
            { firstJobAdvHpGain = 85
            , secondJobAdvHpGain = 325
            , thirdJobAdvHpGain = 1025
            , fourthJobAdvHpGain = 1825
            , naturalHpGain = 66
            , firstJobAdvLevel = 10
            , secondJobAdvLevel = 30
            , thirdJobAdvLevel = 70
            , fourthJobAdvLevel = 120
            }
        archerJobInfo =
            { firstJobAdvHpGain = 162
            , secondJobAdvHpGain = 325
            , thirdJobAdvHpGain = 625
            , fourthJobAdvHpGain = 925
            , naturalHpGain = 22
            , firstJobAdvLevel = 10
            , secondJobAdvLevel = 30
            , thirdJobAdvLevel = 70
            , fourthJobAdvLevel = 120
            }
        thiefJobInfo = archerJobInfo
        buccaneerJobInfo =
            { firstJobAdvHpGain = 162
            , secondJobAdvHpGain = 325
            , thirdJobAdvHpGain = 625
            , fourthJobAdvHpGain = 925
            , naturalHpGain = 55
            , firstJobAdvLevel = 10
            , secondJobAdvLevel = 30
            , thirdJobAdvLevel = 70
            , fourthJobAdvLevel = 120
            }
        corsairJobInfo =
            { buccaneerJobInfo
            | naturalHpGain = 22
            }
        magicianJobInfo =
            { firstJobAdvHpGain = 0
            , secondJobAdvHpGain = 0
            , thirdJobAdvHpGain = 0
            , fourthJobAdvHpGain = 0
            , naturalHpGain = 12
            , firstJobAdvLevel = 8
            , secondJobAdvLevel = 30
            , thirdJobAdvLevel = 70
            , fourthJobAdvLevel = 120
            }
    in
    case job of
        Beginner ->
            calculateBeginnerNaturalHp level
        Warrior ->
            calculateNonBeginnerNaturalHp level warriorJobInfo
        Archer ->
            calculateNonBeginnerNaturalHp level archerJobInfo
        Magician ->
            calculateNonBeginnerNaturalHp level magicianJobInfo
        Thief ->
            calculateNonBeginnerNaturalHp level thiefJobInfo
        Corsair ->
            calculateNonBeginnerNaturalHp level corsairJobInfo
        Buccaneer ->
            calculateNonBeginnerNaturalHp level buccaneerJobInfo

calculateBeginnerNaturalHp : Level.Level -> Int
calculateBeginnerNaturalHp level =
    let
        avgHpGain =
            14
        level1Hp =
            50
    in
    level1Hp + avgHpGain * (level - 1)

calculateNonBeginnerNaturalHp : Level.Level -> JobInfo -> Int
calculateNonBeginnerNaturalHp level jobInfo =
    let
        levelsAsNonBeginner = level - jobInfo.firstJobAdvLevel
    in
    if level < jobInfo.firstJobAdvLevel then
        calculateBeginnerNaturalHp level
    else if level < jobInfo.secondJobAdvLevel then
        calculateBeginnerNaturalHp jobInfo.firstJobAdvLevel
        + jobInfo.firstJobAdvHpGain
        + levelsAsNonBeginner * jobInfo.naturalHpGain
    else if level < jobInfo.thirdJobAdvLevel then
        calculateBeginnerNaturalHp jobInfo.firstJobAdvLevel
        + jobInfo.firstJobAdvHpGain
        + jobInfo.secondJobAdvHpGain
        + levelsAsNonBeginner * jobInfo.naturalHpGain
    else if level < jobInfo.fourthJobAdvLevel then
        calculateBeginnerNaturalHp jobInfo.firstJobAdvLevel
        + jobInfo.firstJobAdvHpGain
        + jobInfo.secondJobAdvHpGain
        + jobInfo.thirdJobAdvHpGain
        + levelsAsNonBeginner * jobInfo.naturalHpGain
    else
        calculateBeginnerNaturalHp jobInfo.firstJobAdvLevel
        + jobInfo.firstJobAdvHpGain
        + jobInfo.secondJobAdvHpGain
        + jobInfo.thirdJobAdvHpGain
        + jobInfo.fourthJobAdvHpGain
        + levelsAsNonBeginner * jobInfo.naturalHpGain