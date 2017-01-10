module Utils
    exposing
        ( area
        , costByDiameter
        , roundFloat
        , stringToFloat
        )

import Round


area : Float -> Float
area radius =
    pi * radius ^ 2


costByDiameter : Float -> Float -> Float
costByDiameter cost diameter =
    cost / (area (diameter / 2))


roundFloat : Float -> String
roundFloat num =
    Round.round 2 num


stringToFloat : String -> Float
stringToFloat str =
    Result.withDefault 0 (String.toFloat str)
