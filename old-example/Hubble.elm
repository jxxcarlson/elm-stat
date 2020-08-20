module Hubble exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Chart exposing (GraphType(..))
import Data exposing (Data, Point, xCoord, yCoord)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import ErrorBars
import Html exposing (Html)
import RawData exposing (RawData)
import SampleData
import Stat
import Style
import TypedSvg
import TypedSvg.Attributes as TA
import TypedSvg.Types as TT


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { chart : Chart.Chart
    , b : Float
    , m : Float
    , r2 : Float
    }


type Msg
    = NoOp


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        data =
            RawData.get SampleData.hubble1929
                |> Maybe.withDefault RawData.empty
                |> RawData.toData 1 2

        ( b, m ) =
            Stat.linearRegression data
                |> Maybe.withDefault ( 0, 1 )

        -- Compute regression line
        x1 =
            RawData.minimum Tuple.first data |> Maybe.withDefault 0

        y1 =
            b + m * x1

        x2 =
            RawData.maximum Tuple.second data |> Maybe.withDefault 1

        y2 =
            b + m * x2

        regressionLine =
            Chart.graph Line 0 0 1 [ ( x1, y1 ), ( x2, y2 ) ]

        r2 =
            Stat.r2 data |> Maybe.withDefault 0

        -- Chart
        chart : Chart.Chart
        chart =
            data
                |> Chart.graph Scatter 1.0 0 0
                |> Chart.chart
                |> Chart.addGraph regressionLine
    in
    ( { chart = chart, b = b, m = m, r2 = r2 }, Cmd.none )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


viewData model =
    TypedSvg.svg [ TA.width (TT.px 900), TA.height (TT.px 600) ] [ Chart.view Nothing model.chart ]
        |> Element.html


view : Model -> Html Msg
view model =
    let
        roundTo : Int -> Float -> Float
        roundTo n x =
            let
                factor =
                    10.0 ^ toFloat n
            in
            x * factor |> round |> toFloat |> (\u -> u / factor)

        mm =
            String.fromFloat (roundTo 1 model.m)

        bb =
            String.fromFloat (roundTo 1 model.b |> abs)

        rr2 =
            String.fromFloat (roundTo 1 model.r2)
    in
    Element.layout mainColumn
        (column
            [ height fill, spacing 8, Font.size 14 ]
            [ viewData model
            , el [ Font.bold, centerX, Font.size 18 ] (text "Hubble galactic recession data")
            , el [ centerX ] (text "x-axis: distance in megaparsecs, y-axis: velocity in km/sec")
            , el [ centerX ] (text ("Regression line: y = " ++ mm ++ "x - " ++ bb ++ ", R2 = " ++ rr2))
            , el [ centerX ] (text ("Hubble constant H ~ " ++ mm ++ " km/sec/megaparsec"))
            , Element.newTabLink [ centerX ]
                { url = "https://www.pnas.org/content/112/11/3173"
                , label = el [ Font.color (Element.rgb 0 0 1) ] (text "Hubble's law and the expanding universe (PNAS)")
                }
            , Element.newTabLink [ centerX ]
                { url = "http://hosting.astro.cornell.edu/academics/courses/astro201/hubbles_law.htm"
                , label = el [ Font.color (Element.rgb 0 0 1) ] (text "Hubble's law (Cornell University)")
                }
            ]
        )


mainColumn =
    [ Background.color (rgb 1.0 1.0 1.0)
    , paddingXY 20 20
    , height fill
    , width fill
    ]
