module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Element.Region as Region
import HistogramChart as HistogramChart
import Html exposing (div)
import Html.Attributes exposing (id, style)
import Random
import StatRandom exposing (generateMatrix, mean, standardNormal)
import SyntaxHighlight exposing (elm, gitHub, toBlockHtml, toInlineHtml, useTheme)
import TypedSvg exposing (svg)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { normalList : List Float
    , uniformList : List Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { normalList = []
      , uniformList = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GenerateNormalList
    | GenerateUniformList
    | NormalList (List Float)
    | UniformList (List Float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateNormalList ->
            ( model
            , Random.generate NormalList (StatRandom.generateList 500 (StatRandom.normal 2 0.5))
            )

        NormalList list ->
            ( { model | normalList = list }
            , Cmd.none
            )

        GenerateUniformList ->
            ( model
            , Random.generate UniformList (StatRandom.generateList 500 (Random.float 0 4))
            )

        UniformList list ->
            ( { model | uniformList = list }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view model =
    Element.layout
        [ Font.size 16
        , Font.family
            [ Font.typeface "Helvetica"
            , Font.sansSerif
            ]
        , height fill
        ]
        (mainRow model)


mainRow model =
    Element.row [ width fill, height fill ] [ leftColumn model, centerColumn model ]


borders =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


paddings =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


leftColumn model =
    Element.column [ height fill, alignTop, width <| px 260, scrollbars, spacing 20, padding 0, Border.widthEach { borders | right = 2 }, Region.navigation ]
        [ paragraph
            [ alignLeft
            , Font.size 18
            , Border.widthEach { borders | bottom = 2 }
            , padding 20
            ]
            [ el [] (text "Welcome to the elm-stat examples!") ]
        , section "Measures of Central Tendency" [ "mean", "mode", "median", "harmonicMean", "geometricMean", "rootMeanSquare", "sampleSkewness" ]
        , section "Measures of Dispersion" [ "variance", "sampleVariance", "standardDeviation", "sampleStandardDeviation", "medianAbsoluteDeviation", "zScore" ]
        , section "Similarity" [ "sampleCorrelation", "sampleCovariance", "rSquared" ]
        , section "Distributions" [ "Uniform", "Normal", "Bernoulli", "Binomial", "Poisson", "tTest" ]
        ]


section a b =
    column [] [ topic a, subTopics b ]


topic name =
    row [ Font.size 16, paddingXY 20 0 ] [ text name ]


subTopics : List String -> Element msg
subTopics names =
    let
        rows =
            List.map createSubTopic names
    in
    column [ Font.size 14, paddingXY 30 5 ] rows


createSubTopic : String -> Element msg
createSubTopic name =
    row
        [ padding 4
        , Border.widthEach { borders | bottom = 1 }
        , Border.color <| rgba 0 0 0 0
        , mouseOver [ Font.size 15, Border.color <| rgb 0 0 0 ]
        , pointer
        ]
        [ text name ]


normal model =
    el [ width shrink, height shrink, Border.width 1, Border.color black, Border.rounded 5, Border.shadow { offset = ( 0, 0 ), size = 10.0, blur = 50.0, color = black }, centerX, padding 30 ]
        (column []
            [ descriptionTitle "Normal Distribution"
            , el [] (syntax "StatRandom.generateList 500 (StatRandom.normal 2 0.5)" |> Element.html)
            , el [ width <| px 900, height fill ] (HistogramChart.view model.normalList |> Element.html)
            , button "Generate" GenerateNormalList
            ]
        )


uniform model =
    el [ width shrink, height shrink, Border.width 1, Border.color black, Border.rounded 5, Border.shadow { offset = ( 0, 0 ), size = 10.0, blur = 50.0, color = black }, centerX, padding 30 ]
        (column []
            [ descriptionTitle "Uniform Distribution"
            , el [ Border.rounded 5 ] (syntax "StatRandom.generateList 500 (Random.float 0 4)" |> Element.html)
            , el [ width <| px 900, height fill ] (HistogramChart.view model.uniformList |> Element.html)
            , button "Generate" GenerateUniformList
            ]
        )



-- info : Html msg


syntax code =
    div []
        [ useTheme gitHub
        , elm code
            |> Result.map toInlineHtml
            |> Result.withDefault (div [] [])
        ]


descriptionTitle title =
    el [ Font.size 18, paddingXY 0 30 ] (text title)


centerColumn model =
    Element.column [ width fill, height fill, centerX, centerY, spacing 36, padding 50, scrollbars ]
        [ uniform model
        , normal model
        ]


blue =
    Element.rgb 0 0 0.8


red =
    Element.rgb 0.8 0 0


black =
    Element.rgba 0 0 0 0.1


white =
    Element.rgb 1 1 1


orange =
    Element.rgba255 247 111 27 0.84


grey =
    Element.rgba255 94 83 83 0.8


button buttonText message =
    Input.button
        [ padding 5
        , Border.width 1
        , Border.rounded 5
        , Border.color black
        , Background.color grey
        , Font.color white
        , mouseOver
            [ Background.color <| rgb255 0xFF 0xFF 0xFF, Font.color <| rgb255 0 0 0 ]
        ]
        { onPress = Just message
        , label = text buttonText
        }
