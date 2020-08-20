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
    }


type Msg
    = NoOp


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        chart : Chart.Chart
        chart =
            RawData.get SampleData.hubble1929
                |> Maybe.withDefault RawData.empty
                |> RawData.toData 1 2
                |> Chart.graph Scatter 1.0 0 0
                |> Chart.chart
    in
    ( { chart = chart }, Cmd.none )


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
    Element.layout mainColumn
        (column
            [ height fill, spacing 8, Font.size 14 ]
            [ viewData model
            , el [ Font.bold, centerX ] (text "Hubble galactic recession data")
            , el [ centerX ] (text "x-axis: distance in megaparsecs, y-axis: velocity in km/sec")
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
