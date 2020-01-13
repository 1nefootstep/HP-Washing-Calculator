module NaturalHpCalculator exposing (calculateNaturalHp)

import Stat.Level as Level exposing (Level)
import Stat.HpStat as HpStat exposing (HpStat)
import Jobs exposing (Job(..), JobAdvancementLevels, JobAdvHpGain)

-- This is the main function in the module
calculateNaturalHp : Job -> Level -> HpStat
calculateNaturalHp job level =
    let
        maybeJobAdvLevels = Jobs.getJobAdvLevels job
        maybeJobAdvHpGain = Jobs.getJobAdvHpGain job
    in
    if Level.lessThan level (Level.fromInt 2) then
        calculateBeginnerNaturalHp (Level.fromInt 1)
    else if job == Beginner then
        calculateBeginnerNaturalHp level
    else
        case maybeJobAdvLevels of
            Nothing ->
                calculateNaturalHp Beginner level
            Just jobAdvLevels ->
                case maybeJobAdvHpGain of
                    Nothing ->
                        calculateNaturalHp Beginner level
                    Just jobAdvHpGain ->
                        calculateNonBeginnerNaturalHp job level jobAdvLevels jobAdvHpGain

-- Everything beneath are helper functions
calculateBeginnerNaturalHp : Level -> HpStat
calculateBeginnerNaturalHp level =
    let
        baseHp = HpStat.fromFloat 50
        beginnerLevelUpHpGain = Jobs.getLevelUpHpGain Beginner
        levelUps = Level.subtract level Level.one
    in
    HpStat.add
        baseHp
        (HpStat.multiplyBy
            beginnerLevelUpHpGain
            (toFloat (Level.toInt levelUps))
        )

calculateLevelsAsBeginnerAndNonBeginner : Level -> Level -> (Level, Level)
calculateLevelsAsBeginnerAndNonBeginner level firstJobAdvLevel =
    let
        differenceInLevels = Level.subtract level firstJobAdvLevel
    in
    if Level.lessThan level Level.zero then
        (level, Level.zero)
    else
        (level, differenceInLevels)

calculateTotalJobAdvHpGain : Level -> JobAdvancementLevels -> JobAdvHpGain -> HpStat
calculateTotalJobAdvHpGain level jobAdvLevels jobAdvHpGain =
    if Level.lessThan level jobAdvLevels.firstJobAdvLevel then
        HpStat.zero
    else if Level.lessThan level jobAdvLevels.secondJobAdvLevel then
        jobAdvHpGain.firstJobAdvHpGain
    else if Level.lessThan level jobAdvLevels.thirdJobAdvLevel then
        List.foldl
            HpStat.add
            HpStat.zero
            [ jobAdvHpGain.firstJobAdvHpGain
            , jobAdvHpGain.secondJobAdvHpGain ]
    else if Level.lessThan level jobAdvLevels.fourthJobAdvLevel then
        List.foldl
            HpStat.add
            HpStat.zero
            [ jobAdvHpGain.firstJobAdvHpGain
            , jobAdvHpGain.secondJobAdvHpGain
            , jobAdvHpGain.thirdJobAdvHpGain ]
    else
        List.foldl
            HpStat.add
            HpStat.zero
            [ jobAdvHpGain.firstJobAdvHpGain
            , jobAdvHpGain.secondJobAdvHpGain
            , jobAdvHpGain.thirdJobAdvHpGain
            , jobAdvHpGain.fourthJobAdvHpGain ]


calculateNonBeginnerNaturalHp : Job -> Level -> JobAdvancementLevels -> JobAdvHpGain -> HpStat
calculateNonBeginnerNaturalHp job level jobAdvLevels jobAdvHpGain =
    let
        (levelsAsBeginner, levelsAsNonBeginner) =
            calculateLevelsAsBeginnerAndNonBeginner level jobAdvLevels.firstJobAdvLevel
        jobHpGainPerLevel =
            Jobs.getLevelUpHpGain job
        jobAdvTotalHpGain =
            calculateTotalJobAdvHpGain level jobAdvLevels jobAdvHpGain
    in
    List.foldl
        HpStat.add
        HpStat.zero
        [ calculateBeginnerNaturalHp levelsAsBeginner
        , HpStat.multiplyBy jobHpGainPerLevel (toFloat (Level.toInt levelsAsNonBeginner))
        , jobAdvTotalHpGain ]



