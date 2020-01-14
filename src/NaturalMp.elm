module NaturalMp exposing (calculateNaturalMp)

import Stat.Level as Level exposing (Level)
import Stat.MpStat as MpStat exposing (MpStat)
import Jobs exposing (Job(..), JobAdvancementLevels, JobAdvMpGain)


-- This is the main function in the module
calculateNaturalMp : Job -> Level -> MpStat
calculateNaturalMp job level =
    let
        maybeJobAdvLevels = Jobs.getJobAdvLevels job
        maybeJobAdvMpGain = Jobs.getJobAdvMpGain job
    in
    if Level.lessThan level (Level.fromInt 2) then
        calculateBeginnerNaturalMp (Level.fromInt 1)
    else if job == Beginner then
        calculateBeginnerNaturalMp level
    else
        case maybeJobAdvLevels of
            Nothing ->
                calculateNaturalMp Beginner level
            Just jobAdvLevels ->
                case maybeJobAdvMpGain of
                    Nothing ->
                        calculateNaturalMp Beginner level
                    Just jobAdvMpGain ->
                        calculateNonBeginnerNaturalMp job level jobAdvLevels jobAdvMpGain

-- Everything beneath are helper functions
calculateBeginnerNaturalMp : Level -> MpStat
calculateBeginnerNaturalMp level =
    let
        baseMp = MpStat.fromFloat 5
        beginnerLevelUpMpGain = Jobs.getLevelUpMpGain Beginner
        levelUps = Level.subtract level Level.one
    in
    MpStat.add
        baseMp
        (MpStat.multiplyBy
            beginnerLevelUpMpGain
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
        (firstJobAdvLevel, differenceInLevels)

calculateTotalJobAdvMpGain : Level -> JobAdvancementLevels -> JobAdvMpGain -> MpStat
calculateTotalJobAdvMpGain level jobAdvLevels jobAdvMpGain =
    if Level.lessThan level jobAdvLevels.firstJobAdvLevel then
        MpStat.zero
    else if Level.lessThan level jobAdvLevels.secondJobAdvLevel then
        jobAdvMpGain.firstJobAdvMpGain
    else if Level.lessThan level jobAdvLevels.thirdJobAdvLevel then
        List.foldl
            MpStat.add
            MpStat.zero
            [ jobAdvMpGain.firstJobAdvMpGain
            , jobAdvMpGain.secondJobAdvMpGain ]
    else if Level.lessThan level jobAdvLevels.fourthJobAdvLevel then
        List.foldl
            MpStat.add
            MpStat.zero
            [ jobAdvMpGain.firstJobAdvMpGain
            , jobAdvMpGain.secondJobAdvMpGain
            , jobAdvMpGain.thirdJobAdvMpGain ]
    else
        List.foldl
            MpStat.add
            MpStat.zero
            [ jobAdvMpGain.firstJobAdvMpGain
            , jobAdvMpGain.secondJobAdvMpGain
            , jobAdvMpGain.thirdJobAdvMpGain
            , jobAdvMpGain.fourthJobAdvMpGain ]

calculateImprovingMpOffset job =
    case job of
        Magician ->
            MpStat.fromFloat -70
        _ ->
            MpStat.zero

calculateNonBeginnerNaturalMp : Job -> Level -> JobAdvancementLevels -> JobAdvMpGain -> MpStat
calculateNonBeginnerNaturalMp job level jobAdvLevels jobAdvMpGain =
    let
        (levelsAsBeginner, levelsAsNonBeginner) =
                (calculateLevelsAsBeginnerAndNonBeginner level jobAdvLevels.firstJobAdvLevel)
        jobMpGainPerLevel =
            Jobs.getLevelUpMpGain job
        jobAdvTotalMpGain =
            calculateTotalJobAdvMpGain level jobAdvLevels jobAdvMpGain
        offset =
            calculateImprovingMpOffset job
    in
    List.foldl
        MpStat.add
        MpStat.zero
        [ calculateBeginnerNaturalMp levelsAsBeginner
        , MpStat.multiplyBy jobMpGainPerLevel (toFloat (Level.toInt levelsAsNonBeginner))
        , jobAdvTotalMpGain
        , offset ]



