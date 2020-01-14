module MpAfterBaseInt exposing (..)

import Stat.Level as Level exposing (Level)
import Stat.MpStat as MpStat exposing (MpStat)
import Stat.Intel as Intel exposing (Intel)

type MWBuff =
    NoMW
    | MW10
    | MW20


calculateAdditionalMpFromIntel : Level -> Intel -> MWBuff -> MpStat
calculateAdditionalMpFromIntel targetLevel targetBaseInt mwBuff =




helperFunction : Level -> Level -> Intel -> Intel -> MWBuff -> MpStat -> MpStat
helperFunction currLevel targetLevel currBaseInt targetBaseInt mwBuff accumulatedMpGain =
    if currLevel < targetLevel then
        helperFunction
            (Level.add currLevel Level.one)
            (targetLevel)
            (levelUpAddIntel currBaseInt targetBaseInt)
            (mwBuff)
            levelUpAddExtraMpFromIntel currBaseInt mwBuff accumulatedMpGain

levelUpAddIntel : Intel -> Intel -> Intel
levelUpAddIntel currBaseInt targetBaseInt =
    let
        diffBetweenCurrAndTarget =
            Intel.subtract targetBaseInt currBaseInt
        fiveInt =
            Intel.fromInt 5
    in
    if (Intel.equalTo diffBetweenCurrAndTarget Intel.zero) then
        currBaseInt
    else if (Intel.lessThan diffBetweenCurrAndTarget fiveInt) then
        Intel.add currBaseInt diffBetweenCurrAndTarget
    else
        Intel.add currBaseInt fiveInt

levelUpAddExtraMpFromIntel : Intel -> MWBuff -> MpStat -> MpStat
levelUpAddExtraMpFromIntel currBaseInt mwBuff accumulatedMpGain =
    case mwBuff of
        NoMW ->
            MpStat.add
                accumulatedMpGain
                (mpFromExtraInt currBaseInt)
        MW10 ->
            MpStat.add
                accumulatedMpGain
                (mpFromExtraInt
                    (multiplyBaseInt currBaseInt 1.05))
        MW20 ->
            MpStat.add
                accumulatedMpGain
                (mpFromExtraInt
                    (multiplyBaseInt currBaseInt 1.1))

mpFromExtraInt : Intel -> MpStat
mpFromExtraInt int =
   MpStat.fromFloat (toFloat (Intel.toInt(Intel.divideBy int 10)))

multiplyBaseInt : Intel -> Float -> Intel
multiplyBaseInt baseInt multiple =
    Intel.fromInt
        (floor
            ((*)
                (toFloat (Intel.toInt baseInt))
                multiple))
