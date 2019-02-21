module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Csv exposing (Csv)
import Display
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes as HA
import Chart
import Utility
import Data exposing (Point, Data, xCoord, yCoord)
import Stat exposing (Statistics, statistics)
import Style
import Svg exposing (Svg)
import Task
import RawData exposing (RawData)
import SampleData


type PlotOption
    = Normal
    | WithRegression


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { filename : Maybe String
    , fileSize : Maybe Int
    , dataText : Maybe String
    , rawData : Maybe RawData
    , data : Data
    , header : Maybe String
    , xMinOriginal : Maybe Float
    , xMaxOriginal : Maybe Float
    , xColumn : Maybe Int
    , yColumn : Maybe Int
    , xMin : Maybe Float
    , xMax : Maybe Float
    , statistics : Maybe Statistics
    , xLabel : Maybe String
    , yLabel : Maybe String
    , plotType : Chart.GraphType
    , plotOption : PlotOption
    , output : String
    }


type Msg
    = NoOp
    | InputXLabel String
    | InputYLabel String
    | InputXMin String
    | InputXMax String
    | InputI String
    | InputJ String
    | FileRequested
    | FileSelected File
    | FileLoaded String
    | SelectLinePlot
    | SelectScatterPlot
    | ToggleRegression
    | Recompute
    | Reset


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { filename = Nothing
      , fileSize = Nothing
      , dataText = Nothing
      , rawData = Nothing
      , data = []
      , xMinOriginal = Nothing
      , xMaxOriginal = Nothing
      , xColumn = Just 0
      , yColumn = Just 1
      , xMax = Nothing
      , xMin = Nothing
      , header = Nothing
      , statistics = Nothing
      , plotType = Chart.Line
      , plotOption = Normal
      , xLabel = Nothing
      , yLabel = Nothing
      , output = "Ready!"
      }
    , Cmd.none
    )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputXLabel str ->
            ( { model | xLabel = Just str }, Cmd.none )

        InputYLabel str ->
            ( { model | yLabel = Just str }, Cmd.none )

        InputXMin str ->
            ( { model | xMin = String.toFloat str }, Cmd.none )

        InputXMax str ->
            ( { model | xMax = String.toFloat str }, Cmd.none )

        InputI str ->
            ( { model | xColumn = (String.toInt str) |> Maybe.map (\x -> x - 1) }, Cmd.none )

        InputJ str ->
            ( { model | yColumn = (String.toInt str) |> Maybe.map (\x -> x - 1) }, Cmd.none )

        FileRequested ->
            ( model
            , Select.file [ "text/*" ] FileSelected
            )

        FileSelected file ->
            ( { model
                | filename = Just <| File.name file
                , fileSize = Just <| File.size file
              }
            , Task.perform FileLoaded (File.toString file)
            )

        SelectLinePlot ->
            ( { model | plotType = Chart.Line }, Cmd.none )

        SelectScatterPlot ->
            ( { model | plotType = Chart.Scatter }, Cmd.none )

        ToggleRegression ->
            case model.plotOption of
                Normal ->
                    ( { model | plotOption = WithRegression }, Cmd.none )

                WithRegression ->
                    ( { model | plotOption = Normal }, Cmd.none )

        Reset ->
            let
                nextModel =
                    { model | xMin = model.xMinOriginal, xMax = model.xMaxOriginal }

                ( numericalData, statistics ) =
                    recompute nextModel
            in
                ( { nextModel | data = numericalData, statistics = statistics }, Cmd.none )

        Recompute ->
            let
                ( numericalData, statistics ) =
                    recompute model
            in
                ( { model | data = numericalData, statistics = statistics }, Cmd.none )

        FileLoaded content ->
            let
                rawData =
                    RawData.get content

                xLabel =
                    case rawData of
                        Nothing ->
                            Nothing

                        Just data ->
                            Utility.listGetAt 0 data.columnHeaders

                yLabel =
                    case rawData of
                        Nothing ->
                            Nothing

                        Just data ->
                            Utility.listGetAt 1 data.columnHeaders

                numericalData =
                    case rawData of
                        Nothing ->
                            []

                        Just rawData_ ->
                            (RawData.toData 0 1 rawData_)
                                |> Maybe.withDefault []

                statistics =
                    case numericalData of
                        [] ->
                            Nothing

                        dataList ->
                            Stat.statistics dataList
            in
                ( { model
                    | dataText = Just content
                    , rawData = rawData
                    , data = numericalData
                    , header = Maybe.map .metadata rawData |> Maybe.map (String.join "\n")
                    , xLabel = xLabel
                    , yLabel = yLabel
                    , xMin = Maybe.map .xMin statistics
                    , xMax = Maybe.map .xMax statistics
                    , xMinOriginal = Maybe.map .xMin statistics
                    , xMaxOriginal = Maybe.map .xMax statistics
                    , statistics = statistics
                  }
                , Cmd.none
                )


recompute : Model -> ( Data, Maybe Statistics )
recompute model =
    let
        data =
            Debug.log "FINAL DATA" <|
                case model.rawData of
                    Nothing ->
                        []

                    Just rawData ->
                        let
                            i =
                                model.xColumn |> Maybe.withDefault 0

                            j =
                                model.yColumn |> Maybe.withDefault 1
                        in
                            Debug.log "DATA" (RawData.toData i j rawData)
                                |> Maybe.withDefault []
                                |> Stat.filter { xMin = model.xMin, xMax = model.xMax }

        statistics =
            case data of
                [] ->
                    Nothing

                dataList ->
                    Stat.statistics dataList
    in
        ( data, statistics )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout Style.outer
        (column
            [ height fill ]
            [ mainRow model
            , footer model
            ]
        )


mainRow : Model -> Element Msg
mainRow model =
    row [ spacing 24, alignTop ]
        [ dataColumn model
        , statisticsPanel model
        , rightColumn model
        ]


rightColumn : Model -> Element Msg
rightColumn model =
    column [ spacing 8, moveUp 90 ]
        [ visualDataDisplay model
        , row [ moveDown 100, spacing 24 ]
            [ row [ spacing 12 ] [ linePlotButton model, scatterPlotButton model ]
            , toggleRegressionButton model
            ]
        , column
            [ spacing 8
            , Font.size 11
            , moveRight 50
            , moveUp 105
            ]
            [ el
                [ scrollbarY
                , scrollbarX
                , height (px 95)
                , width (px 800)
                , padding 8
                , Background.color (rgb255 255 255 255)
                , moveDown 220
                , moveLeft 50
                ]
                (text <| headerString model)
            ]
        ]


headerString : Model -> String
headerString model =
    case model.header of
        Nothing ->
            "No header"

        Just str ->
            str


dataColumn : Model -> Element Msg
dataColumn model =
    column Style.mainColumn
        [ column [ spacing 20 ]
            [ column [ spacing 8 ] [ title "Data Explorer", openFileButton ]
            , column
                [ spacing 8 ]
                [ inputXLabel model, inputYLabel model ]
            , rawDataDisplay model
            ]
        ]


footer : Model -> Element Msg
footer model =
    row Style.footer
        [ downloadSampleCsvFile
        , el [] (text <| "File: " ++ (Display.label "-" model.filename))
        , el [] (text <| fileSizeString model.fileSize)
        ]


fileSizeString : Maybe Int -> String
fileSizeString k =
    let
        maybeSize =
            Maybe.map2 (++) (Maybe.map String.fromInt k) (Just " bytes")
    in
        "Size: " ++ Display.label "-" maybeSize


downloadSampleCsvFile : Element Msg
downloadSampleCsvFile =
    download Style.link
        { url = "https://jxxcarlson.github.io/app/temperature-anomalies.csv"
        , label = el [] (text "Download sample data.csv file")
        }



--
-- CHART
--


visualDataDisplay : Model -> Element msg
visualDataDisplay model =
    let
        regressionGraph =
            case ( model.statistics, model.plotOption ) of
                ( Just stats, WithRegression ) ->
                    Just <|
                        Chart.graph model.plotType 0 0 1 <|
                            [ stats.leftRegressionPoint, stats.rightRegressionPoint ]

                ( _, _ ) ->
                    Nothing

        chart1 =
            Chart.chart <| Chart.graph model.plotType 1 0 0 model.data

        chart2 =
            case regressionGraph of
                Nothing ->
                    chart1

                Just rg ->
                    Chart.addGraph rg chart1
    in
        row
            [ Font.size 12
            , width (px 800)
            , height (px 515)
            , Background.color <| rgb255 255 255 255
            , padding 30
            , moveDown 95
            ]
            [ Element.html (Chart.view <| chart2) ]



--
-- STATISTICS
--


statisticsPanel : Model -> Element Msg
statisticsPanel model =
    column
        [ spacing 12
        , Font.size 12
        , Background.color (rgb255 245 245 245)
        , width (px 200)
        , height (px 675)
        , paddingXY 8 12
        , moveDown 15
        ]
        [ el []
            (text <| numberOfRecordsString model.data)
        , el []
            (text <| plotTypeAsString model.plotType)
        , showIfNot (model.rawData == Nothing) <| xInfoDisplay model
        , showIfNot (model.rawData == Nothing) <| Display.info "y" model.yLabel yCoord model.data
        , showIfNot (model.rawData == Nothing) <| Display.correlationInfo model.data
        , showIfNot (model.rawData == Nothing) <| el [ Font.bold, paddingXY 0 5 ] (text <| "TOOLS")
        , showIfNot (model.rawData == Nothing) <| inputXMin model
        , showIfNot (model.rawData == Nothing) <| inputXMax model
        , showIfNot (model.rawData == Nothing) <| row [ spacing 12 ] [ el [ Font.bold ] (text <| "Column"), inputI model, inputJ model ]
        , showIfNot (model.rawData == Nothing) <| recomputeButton
        , showIfNot (model.rawData == Nothing) <| resetButton
        ]


showIf : Bool -> Element msg -> Element msg
showIf condition element =
    case condition of
        True ->
            element

        False ->
            Element.none


showIfNot : Bool -> Element msg -> Element msg
showIfNot condition element =
    case condition of
        True ->
            Element.none

        False ->
            element


xInfoDisplay : Model -> Element msg
xInfoDisplay model =
    case model.plotType of
        Chart.Line ->
            Display.smallInfo "x" model.xLabel xCoord model.data

        Chart.Scatter ->
            Display.info "x" model.xLabel xCoord model.data


plotTypeAsString : Chart.GraphType -> String
plotTypeAsString graphType =
    case graphType of
        Chart.Line ->
            "Plot: line"

        Chart.Scatter ->
            "Plot: scatter"



--
-- RAW DATA DISPLAY
--


numberOfRecordsString : Data -> String
numberOfRecordsString data =
    "Records: " ++ String.fromInt (List.length data)


title : String -> Element msg
title str =
    row [ centerX, Font.bold ] [ text str ]


rawDataDisplay : Model -> Element msg
rawDataDisplay model =
    table Style.table
        { data = model.data
        , columns =
            [ { header = Element.text (Display.label "x" model.xLabel)
              , width = fill
              , view =
                    \point ->
                        Element.text <| Display.stringOfFloat (xCoord point)
              }
            , { header = Element.text (Display.label "y" model.yLabel)
              , width = fill
              , view =
                    \point ->
                        Element.text <| Display.stringOfFloat (yCoord point)
              }
            ]
        }



--
-- INPUT FIELDS
--


inputI : Model -> Element Msg
inputI model =
    Input.text [ width (px 36), height (px 18), Font.size 12, paddingXY 8 0 ]
        { onChange = InputI
        , text =
            Display.label "column i"
                (Maybe.map String.fromInt (Maybe.map (\n -> n + 1) model.xColumn))
        , placeholder = Nothing
        , label = Input.labelLeft [ moveDown 4 ] <| el [] (text "x:")
        }


inputJ : Model -> Element Msg
inputJ model =
    Input.text [ width (px 36), height (px 18), Font.size 12, paddingXY 8 0 ]
        { onChange = InputJ
        , text =
            Display.label "column j"
                (Maybe.map String.fromInt (Maybe.map (\n -> n + 1) model.yColumn))
        , placeholder = Nothing
        , label = Input.labelLeft [ moveDown 4 ] <| el [] (text "y:")
        }


inputXMin : Model -> Element Msg
inputXMin model =
    Input.text [ height (px 18), Font.size 12, paddingXY 8 0 ]
        { onChange = InputXMin
        , text = Display.label "xMin ..." (Maybe.map String.fromFloat model.xMin)
        , placeholder = Nothing
        , label = Input.labelLeft [ moveDown 4 ] <| el [] (text "x min:")
        }


inputXMax : Model -> Element Msg
inputXMax model =
    Input.text [ height (px 18), Font.size 12, paddingXY 8 0 ]
        { onChange = InputXMax
        , text = Display.label "xMax ..." (Maybe.map String.fromFloat model.xMax)
        , placeholder = Nothing
        , label = Input.labelLeft [ moveDown 4 ] <| el [] (text "x max:")
        }


inputXLabel : Model -> Element Msg
inputXLabel model =
    let
        labelText =
            case model.xLabel of
                Nothing ->
                    "Label for x values"

                Just str ->
                    str
    in
        Input.text [ height (px 18), Font.size 12, paddingXY 8 0 ]
            { onChange = InputXLabel
            , text = labelText
            , placeholder = Nothing
            , label = Input.labelLeft [ moveDown 4 ] <| el [] (text "X:")
            }


inputYLabel : Model -> Element Msg
inputYLabel model =
    let
        labelText =
            case model.yLabel of
                Nothing ->
                    "Label for y values"

                Just str ->
                    str
    in
        Input.text [ height (px 18), Font.size 12, paddingXY 8 0, width (px 185) ]
            { onChange = InputYLabel
            , text = labelText
            , placeholder = Nothing
            , label = Input.labelLeft [] <| el [ moveDown 4 ] (text "Y:")
            }



--
-- BUTTONS
--


openFileButton : Element Msg
openFileButton =
    row [ centerX ]
        [ Input.button Style.button
            { onPress = Just FileRequested
            , label = el [] (text "Open data file")
            }
        ]


activeBackground : Bool -> Attr decorative msg
activeBackground flag =
    case flag of
        True ->
            Style.buttonActiveBackground

        False ->
            Style.buttonBackground


toggleRegressionButton : Model -> Element Msg
toggleRegressionButton model =
    row [ centerX ]
        [ Input.button (Style.plainButton ++ [ activeBackground (model.plotOption == WithRegression) ])
            { onPress = Just ToggleRegression
            , label = el [] (text "Regression line")
            }
        ]


linePlotButton : Model -> Element Msg
linePlotButton model =
    row [ centerX ]
        [ Input.button (Style.plainButton ++ [ activeBackground (model.plotType == Chart.Line) ])
            { onPress = Just SelectLinePlot
            , label = el [] (text "Line")
            }
        ]


scatterPlotButton : Model -> Element Msg
scatterPlotButton model =
    row [ centerX ]
        [ Input.button (Style.plainButton ++ [ activeBackground (model.plotType == Chart.Scatter) ])
            { onPress = Just SelectScatterPlot
            , label = el [] (text "Scatter")
            }
        ]


recomputeButton : Element Msg
recomputeButton =
    row [ centerX ]
        [ Input.button Style.button
            { onPress = Just Recompute
            , label = el [ centerX, width (px 85) ] (text "Recompute")
            }
        ]


resetButton : Element Msg
resetButton =
    row [ centerX ]
        [ Input.button Style.button
            { onPress = Just Reset
            , label = el [ centerX, width (px 85) ] (text "Reset")
            }
        ]
