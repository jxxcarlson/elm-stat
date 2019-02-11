module Display exposing (correlationInfo, info, label, smallInfo, stringOfFloat)

import Element exposing (..)
import Element.Font as Font
import Stat exposing (Data, Point)


info : String -> Maybe String -> (Point -> Float) -> Data -> Element msg
info defaultLabel maybeLabel selector data =
    column [ spacing 5 ]
        [ el [ Font.bold ] (text <| label defaultLabel maybeLabel)
        , el []
            (text <| displayAverage selector data)
        , el []
            (text <| displayStdev selector data)
        , el []
            (text <| displayMinimum selector data)
        , el []
            (text <| displayMaximum selector data)
        ]


smallInfo : String -> Maybe String -> (Point -> Float) -> Data -> Element msg
smallInfo defaultLabel maybeLabel selector data =
    column [ spacing 5 ]
        [ el [ Font.bold ] (text <| label defaultLabel maybeLabel)
        , el []
            (text <| displayMinimum selector data)
        , el []
            (text <| displayMaximum selector data)
        ]


correlationInfo : Data -> Element msg
correlationInfo data =
    let
        statistics =
            Stat.statistics data
    in
    case statistics of
        Nothing ->
            Element.none

        Just stats ->
            column [ spacing 5 ]
                [ el [ Font.bold ] (text <| "Correlation info (y = mx + b)")
                , el []
                    (text <| "m: " ++ stringOfFloat stats.m)
                , el []
                    (text <| "b: " ++ stringOfFloat stats.b)
                , el []
                    (text <| "R2: " ++ stringOfFloat stats.r2)
                ]



--
-- HELPERS
--


stringOfFloat : Float -> String
stringOfFloat value =
    String.left 6 <| String.fromFloat value


label : String -> Maybe String -> String
label defaultLabel maybeLabel =
    case maybeLabel of
        Nothing ->
            defaultLabel

        Just str ->
            if str == "" then
                defaultLabel

            else
                str


displayMinimum : (Point -> Float) -> Data -> String
displayMinimum selector data =
    case Stat.minimum selector data of
        Nothing ->
            "min: ?"

        Just value ->
            "min: " ++ stringOfFloat value


displayMaximum : (Point -> Float) -> Data -> String
displayMaximum selector data =
    case Stat.maximum selector data of
        Nothing ->
            "max: ?"

        Just value ->
            "max: " ++ stringOfFloat value


displayAverage : (Point -> Float) -> Data -> String
displayAverage selector data =
    case Stat.average selector data of
        Nothing ->
            "average: ?"

        Just value ->
            "average: " ++ stringOfFloat value


displayStdev : (Point -> Float) -> Data -> String
displayStdev selector data =
    case Stat.stdev selector data of
        Nothing ->
            "stdev: ?"

        Just value ->
            "stdev: " ++ stringOfFloat value
