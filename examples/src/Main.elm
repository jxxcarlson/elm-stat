module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import HistogramChart as HistogramChart
import Html exposing (Html, div)
import Html.Attributes exposing (href, id)
import Random
import Stat as Stat
import StatRandom as StatRandom
import SyntaxHighlight exposing (elm, gitHub, toInlineHtml, useTheme)



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
    , exponentialList : List Float
    , betaList : List Float
    , bernoulliList : List Int
    , binomialList : List Int
    , poissonList : List Int
    , geometricList : List Int
    , centralText : String
    , centralList : List Float
    , dispersionText : String
    , dispersionList : List Float
    , similarityInput1 : String
    , similarityInput2 : String
    , similarityInput : List ( Float, Float )
    , regressionInput1 : String
    , regressionInput2 : String
    , regressionInput : List ( Float, Float )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { normalList = []
      , uniformList = []
      , exponentialList = []
      , betaList = []
      , bernoulliList = []
      , binomialList = []
      , poissonList = []
      , geometricList = []
      , centralText = "1, 2, 3, 4, 5"
      , centralList = [ 1, 2, 3, 4, 5 ]
      , dispersionText = "0, 10, 20, 30"
      , dispersionList = [ 0, 10, 20, 30 ]
      , similarityInput1 = "1,2,3"
      , similarityInput2 = "5,10,15"
      , similarityInput = [ ( 1, 5 ), ( 2, 10 ), ( 3, 15 ) ]
      , regressionInput1 = "1,2,3"
      , regressionInput2 = "5,10,15"
      , regressionInput = [ ( 1, 5 ), ( 2, 10 ), ( 3, 15 ) ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GenerateNormalList
    | GenerateUniformList
    | GenerateExponentialList
    | GenerateBetaList
    | GenerateBenroulliBoolList
    | GenerateBinomialList
    | GeneratePoissonList
    | GenerateGeometricList
    | NormalList (List Float)
    | UniformList (List Float)
    | ExponentialList (List Float)
    | BetaList (List Float)
    | BernoulliList (List Int)
    | BinomialList (List Int)
    | PoissonList (List Int)
    | GeometricList (List Int)
    | SetCentralText String
    | SetDispersionText String
    | SetSimilarityInput1 String
    | SetSimilarityInput2 String
    | SetRegressionInput1 String
    | SetRegressionInput2 String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateNormalList ->
            ( model
            , Random.generate NormalList (StatRandom.generateList 1000 (StatRandom.normal 2 0.5))
            )

        NormalList list ->
            ( { model | normalList = list }
            , Cmd.none
            )

        GenerateUniformList ->
            ( model
            , Random.generate UniformList (StatRandom.generateList 1000 (Random.float 0 4))
            )

        UniformList list ->
            ( { model | uniformList = list }
            , Cmd.none
            )

        GenerateExponentialList ->
            ( model
            , Random.generate ExponentialList (StatRandom.generateList 1000 (StatRandom.exponential 1.5))
            )

        ExponentialList list ->
            ( { model | exponentialList = list }
            , Cmd.none
            )

        GenerateBetaList ->
            ( model
            , Random.generate BetaList (StatRandom.generateList 1000 (StatRandom.beta 2 3))
            )

        BetaList list ->
            ( { model | betaList = list }
            , Cmd.none
            )

        GenerateBenroulliBoolList ->
            ( model
            , Random.generate BernoulliList (StatRandom.generateList 1000 (StatRandom.bernoulliInt 0.3))
            )

        BernoulliList list ->
            ( { model | bernoulliList = list }
            , Cmd.none
            )

        GenerateBinomialList ->
            ( model
            , Random.generate BinomialList (StatRandom.generateList 1000 (StatRandom.binomial 0.5 20))
            )

        BinomialList list ->
            ( { model | binomialList = list }
            , Cmd.none
            )

        GeneratePoissonList ->
            ( model
            , Random.generate PoissonList (StatRandom.generateList 1000 (StatRandom.poisson 0.8 10))
            )

        PoissonList list ->
            ( { model | poissonList = list }
            , Cmd.none
            )

        GenerateGeometricList ->
            ( model
            , Random.generate GeometricList (StatRandom.generateList 1000 (StatRandom.geometric 0.4 10))
            )

        GeometricList list ->
            ( { model | geometricList = list }
            , Cmd.none
            )

        SetCentralText string ->
            ( { model | centralText = string, centralList = stringToFloatList string }
            , Cmd.none
            )

        SetDispersionText string ->
            ( { model | dispersionText = string, dispersionList = stringToFloatList string }
            , Cmd.none
            )

        SetSimilarityInput1 string ->
            ( { model | similarityInput1 = string, similarityInput = stringsToTuple string model.similarityInput2 }
            , Cmd.none
            )

        SetSimilarityInput2 string ->
            ( { model | similarityInput2 = string, similarityInput = stringsToTuple model.similarityInput1 string }
            , Cmd.none
            )

        SetRegressionInput1 string ->
            ( { model | regressionInput1 = string, regressionInput = stringsToTuple string model.regressionInput2 }
            , Cmd.none
            )

        SetRegressionInput2 string ->
            ( { model | regressionInput2 = string, regressionInput = stringsToTuple model.regressionInput1 string }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
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


mainRow : Model -> Element Msg
mainRow model =
    Element.row [ width fill, height fill ] [ leftColumn model, mainColumn model ]


borders =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


leftColumn : Model -> Element Msg
leftColumn model =
    Element.column [ height fill, alignTop, width <| px 260, scrollbars, spacing 20, padding 0, Border.widthEach { borders | right = 2 }, Region.navigation ]
        [ paragraph
            [ alignLeft
            , Font.size 18
            , Border.widthEach { borders | bottom = 2 }
            , padding 20
            ]
            [ el [] (text "Welcome to the elm-stat examples!") ]
        , sectionId "Measures of Central Tendency" [ "mean", "average", "weightedMean", "harmonicMean", "geometricMean", "mode", "median", "rootMeanSquare", "sampleSkewness" ] "cen"
        , sectionId "Measures of Dispersion" [ "variance", "standardDeviation", "meanAbsoluteDeviation", "medianAbsoluteDeviation", "zScore", "zScores" ] "dis"
        , sectionId "Similarity" [ "covariance", "correlation", "r2" ] "sim"
        , sectionId "Linear regression" [ "linearRegression", "linearRegressionLine" ] "lr"
        , section "Distributions" [ "Bernoulli", "Binomial", "Poisson", "Geometric", "Uniform", "Normal", "Standard Normal", "Exponential", "Beta" ]
        ]


section : String -> List String -> Element Msg
section topicName subTopicNames =
    Element.column [] [ topic topicName, subTopics subTopicNames ]


sectionId : String -> List String -> String -> Element Msg
sectionId topicName subTopicNames id_ =
    Element.column [] [ topic topicName, subTopicsId subTopicNames id_ ]


topic : String -> Element Msg
topic name =
    Element.row [ Font.size 16, paddingXY 20 0 ] [ text name ]


subTopics : List String -> Element msg
subTopics names =
    let
        rows =
            List.map (\x -> createSubTopic x x) names
    in
    column [ Font.size 14, paddingXY 30 5 ] rows


subTopicsId : List String -> String -> Element msg
subTopicsId names id_ =
    let
        rows =
            List.map (\x -> createSubTopic x id_) names
    in
    column [ Font.size 14, paddingXY 30 5 ] rows


createSubTopic : String -> String -> Element msg
createSubTopic name id_ =
    row
        [ padding 4
        , Border.widthEach { borders | bottom = 1 }
        , Border.color <| rgba 0 0 0 0
        , mouseOver [ Font.size 15, Border.color <| rgb 0 0 0 ]
        , pointer
        ]
        [ link []
            { url = "#" ++ id_
            , label = text name
            }
        ]


bernoulli : Model -> Element Msg
bernoulli model =
    box "Bernoulli"
        [ descriptionTitle "Bernoulli Distribution"
        , el [ Border.rounded 5, paddingXY 0 10 ] (syntax "StatRandom.generateList 1000 (Random.bernoulliInt 0.3)" |> Element.html)
        , el [ width <| px 900, height fill ] (HistogramChart.view (List.map toFloat model.bernoulliList) ( -1, 1 ) 2 0 |> Element.html)
        , button "Generate" GenerateBenroulliBoolList
        ]


binomial : Model -> Element Msg
binomial model =
    box "Binomial"
        [ descriptionTitle "Binomial Distribution"
        , el [ Border.rounded 5, paddingXY 0 10 ] (syntax "StatRandom.generateList 1000 (Random.binomial 0.5 20)" |> Element.html)
        , el [ width <| px 900, height fill ] (HistogramChart.view (List.map toFloat model.binomialList) ( 0, 20 ) 20 0 |> Element.html)
        , button "Generate" GenerateBinomialList
        ]


poisson : Model -> Element Msg
poisson model =
    box "Poisson"
        [ descriptionTitle "Poisson Distribution"
        , el [ Border.rounded 5, paddingXY 0 10 ] (syntax "StatRandom.generateList 1000 (Random.poisson 0.8 10)" |> Element.html)
        , el [ width <| px 900, height fill ] (HistogramChart.view (List.map toFloat model.poissonList) ( -1, 10 ) 11 0 |> Element.html)
        , button "Generate" GeneratePoissonList
        ]


geometric : Model -> Element Msg
geometric model =
    box "Geometric"
        [ descriptionTitle "Geometric Distribution"
        , el [ Border.rounded 5, paddingXY 0 10 ] (syntax "StatRandom.generateList 1000 (Random.geometric 0.4 10)" |> Element.html)
        , el [ width <| px 900, height fill ] (HistogramChart.view (List.map toFloat model.geometricList) ( 0, 10 ) 11 0 |> Element.html)
        , button "Generate" GenerateGeometricList
        ]


normal : Model -> Element Msg
normal model =
    box "Normal"
        [ descriptionTitle "Normal Distribution"
        , el [] (syntax "StatRandom.generateList 1000 (StatRandom.normal 2 0.5)" |> Element.html)
        , el [ width <| px 900, height fill ] (HistogramChart.view model.normalList ( 0, 4 ) 20 20 |> Element.html)
        , button "Generate" GenerateNormalList
        ]


uniform : Model -> Element Msg
uniform model =
    box "Uniform"
        [ descriptionTitle "Uniform Distribution"
        , el [ Border.rounded 5 ] (syntax "StatRandom.generateList 1000 (StatRandom.float 0 4)" |> Element.html)
        , el [ width <| px 900, height fill ] (HistogramChart.view model.uniformList ( 0, 4 ) 20 20 |> Element.html)
        , button "Generate" GenerateUniformList
        ]


exponential : Model -> Element Msg
exponential model =
    box "Exponential"
        [ descriptionTitle "Exponential Distribution"
        , el [ Border.rounded 5 ] (syntax "StatRandom.generateList 1000 (StatRandom.exponential 1.0)" |> Element.html)
        , el [ width <| px 900, height fill ] (HistogramChart.view model.exponentialList ( 0, 4 ) 20 20 |> Element.html)
        , button "Generate" GenerateExponentialList
        ]


beta : Model -> Element Msg
beta model =
    box "Beta"
        [ descriptionTitle "Beta Distribution"
        , el [ Border.rounded 5 ] (syntax "StatRandom.generateList 1000 (StatRandom.beta 2 3)" |> Element.html)
        , el [ width <| px 900, height fill ] (HistogramChart.view model.betaList ( 0, 2 ) 20 20 |> Element.html)
        , button "Generate" GenerateBetaList
        ]


options string func =
    { onChange = func
    , text = string
    , placeholder = Just (Input.placeholder [] (text ""))
    , label = Input.labelAbove [] none
    , spellcheck = False
    }


stringToFloatList : String -> List Float
stringToFloatList s =
    String.replace " " "" s |> String.split "," |> List.map (\x -> Maybe.withDefault 0 (String.toFloat x))


stringsToTuple : String -> String -> List ( Float, Float )
stringsToTuple s1 s2 =
    List.map2 Tuple.pair (stringToFloatList s1) (stringToFloatList s2)


measuresOfCentralTendency : Model -> Element Msg
measuresOfCentralTendency model =
    box "cen"
        [ descriptionTitle "Measures of Central Tendency"
        , multilineInput model.centralText SetCentralText model.centralList
        , exampleRow "Mode" Stat.mode model.centralList
        , exampleRow "Average" Stat.mean model.centralList
        , exampleRow "Geometric Mean" Stat.geometricMean model.centralList
        , exampleRow "Harmonic Mean" Stat.harmonicMean model.centralList
        , exampleRow "Root Mean Square" Stat.rootMeanSquare model.centralList
        , exampleRow "Median" Stat.median model.centralList
        , exampleRow "Skewness" Stat.skewness model.centralList
        ]


multilineInput : String -> (String -> Msg) -> List a -> Element Msg
multilineInput content func list =
    row [ width fill, paddingXY 0 20 ]
        [ el [ width <| fillPortion 2 ] (Input.multiline [] (options content func))
        , el [ width <| fillPortion 2, paddingXY 20 0 ] (text ("Your list: " ++ Debug.toString list))
        ]


tupleInput : String -> (String -> Msg) -> String -> (String -> Msg) -> List a -> Element Msg
tupleInput content1 func1 content2 func2 list =
    column [ paddingXY 0 20, width fill ]
        [ row [ paddingXY 0 10, width fill ]
            [ el [ width fill ] (Input.multiline [] (options content1 func1))
            , el [ width fill ] (Input.multiline [] (options content2 func2))
            ]
        , text ("Your tuple list: " ++ Debug.toString list)
        ]


dispersion : Model -> Element Msg
dispersion model =
    box "dis"
        [ descriptionTitle "Measures of Dispersion"
        , multilineInput model.dispersionText SetDispersionText model.dispersionList
        , exampleRow "Variance" Stat.variance model.dispersionList
        , exampleRow "Standard Deviation" Stat.standardDeviation model.dispersionList
        , exampleRow "Mean Absolute Deviation" Stat.meanAbsoluteDeviation model.dispersionList
        , exampleRow "Median Absolute Deviation" Stat.medianAbsoluteDeviation model.dispersionList
        , exampleLongRow "Z scores" Stat.zScores model.dispersionList
        ]


similarity : Model -> Element Msg
similarity model =
    box "sim"
        [ descriptionTitle "Similarity"
        , tupleInput model.similarityInput1 SetSimilarityInput1 model.similarityInput2 SetSimilarityInput2 model.similarityInput
        , exampleRow "Covariance" Stat.covariance model.similarityInput
        , exampleRow "Correlation" Stat.correlation model.similarityInput
        , exampleRow "r2" Stat.r2 model.similarityInput
        ]


box : String -> List (Element Msg) -> Element Msg
box id_ cc =
    el [ width (px 1000), height shrink, Border.width 1, Border.color black, Border.rounded 5, Border.shadow { offset = ( 0, 0 ), size = 10.0, blur = 50.0, color = black }, centerX, padding 30 ]
        (column [ htmlAttribute (id id_) ]
            cc
        )


linearReg : Model -> Element Msg
linearReg model =
    box "lr"
        [ descriptionTitle "Linear Regression"
        , tupleInput model.regressionInput1 SetRegressionInput1 model.regressionInput2 SetRegressionInput2 model.regressionInput
        , exampleRow "linear regression" Stat.linearRegression model.regressionInput
        ]


exampleRow : String -> (List a -> Maybe b) -> List a -> Element Msg
exampleRow funcName func list =
    let
        res =
            func list
    in
    row [ paddingXY 0 10, width <| px 800 ]
        [ text
            (funcName
                ++ ": "
                ++ (case res of
                        Nothing ->
                            "NaN"

                        Just x ->
                            Debug.toString x
                   )
            )
        ]


exampleLongRow : String -> (List a -> Maybe (List Float)) -> List a -> Element Msg
exampleLongRow funcName func list =
    let
        res =
            func list
    in
    row [ paddingXY 0 10, width <| px 800 ]
        [ text
            (funcName
                ++ ": "
                ++ (case res of
                        Nothing ->
                            "NaN"

                        Just xs ->
                            List.map (\x -> String.fromFloat x |> String.left 6) xs |> String.join ", "
                   )
            )
        ]


syntax : String -> Html msg
syntax code =
    div []
        [ useTheme gitHub
        , elm code
            |> Result.map toInlineHtml
            |> Result.withDefault (div [] [])
        ]


descriptionTitle : String -> Element Msg
descriptionTitle title =
    el [ Font.size 24, paddingXY 0 30, Font.underline ] (text title)


mainColumn : Model -> Element Msg
mainColumn model =
    Element.column [ width fill, height fill, centerX, centerY, spacing 36, padding 50, scrollbars ]
        [ measuresOfCentralTendency model
        , dispersion model
        , similarity model
        , linearReg model
        , bernoulli model
        , binomial model
        , poisson model
        , geometric model
        , uniform model
        , normal model
        , exponential model
        , beta model
        ]


black : Color
black =
    Element.rgba 0 0 0 0.1


white : Color
white =
    Element.rgb 1 1 1


grey : Color
grey =
    Element.rgba255 94 83 83 0.8


button : String -> msg -> Element msg
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
