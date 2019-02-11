module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Csv exposing (Csv)
import CsvData
import Display
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes as HA
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import List.Extra
import Maybe.Extra
import Stat exposing (Data, Point, Statistics, statistics)
import Style
import Svg exposing (Svg)
import Task


type PlotType
    = TimeSeries
    | ScatterPlot


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { filename : String
    , csvText : Maybe String
    , csvData : Maybe Csv
    , data : Data
    , header : Maybe String
    , xMinOriginal : Maybe Float
    , xMaxOriginal : Maybe Float
    , xMin : Maybe Float
    , xMax : Maybe Float
    , statistics : Maybe Statistics
    , xLabel : Maybe String
    , yLabel : Maybe String
    , plotType : PlotType
    , output : String
    }


type Msg
    = NoOp
    | InputXLabel String
    | InputYLabel String
    | InputXMin String
    | InputXMax String
    | CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | Recompute
    | Reset


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { filename = "no file yet"
      , csvText = Nothing
      , csvData = Nothing
      , data = []
      , xMinOriginal = Nothing
      , xMaxOriginal = Nothing
      , xMax = Nothing
      , xMin = Nothing
      , header = Nothing
      , statistics = Nothing
      , plotType = TimeSeries
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

        CsvRequested ->
            ( model
            , Select.file [ "text/csvText" ] CsvSelected
            )

        CsvSelected file ->
            ( model
            , Task.perform CsvLoaded (File.toString file)
            )

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

        CsvLoaded content ->
            let
                ( csvData, header ) =
                    CsvData.intelligentGet "," content

                xLabel =
                    case csvData of
                        Nothing ->
                            Nothing

                        Just data ->
                            List.Extra.getAt 0 data.headers

                yLabel =
                    case csvData of
                        Nothing ->
                            Nothing

                        Just data ->
                            List.Extra.getAt 1 data.headers

                numericalData =
                    case csvData of
                        Nothing ->
                            []

                        Just data ->
                            CsvData.getPointList 0 1 data

                statistics =
                    case numericalData of
                        [] ->
                            Nothing

                        dataList ->
                            Stat.statistics dataList
            in
            ( { model
                | csvText = Just content
                , csvData = csvData
                , data = numericalData
                , header = header
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
        numericalData =
            case model.csvData of
                Nothing ->
                    []

                Just data ->
                    CsvData.getPointList 0 1 data
                        |> Stat.filterData { xMin = model.xMin, xMax = model.xMax }

        statistics =
            case numericalData of
                [] ->
                    Nothing

                dataList ->
                    Stat.statistics dataList
    in
    ( numericalData, statistics )



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
    column [ spacing 8 ]
        [ visualDataDisplay model
        , el [ Font.size 11, moveRight 50, moveUp 70 ] (text <| headerString model)
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
        [ downloadSampleCsvFile ]


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
    row
        [ Font.size 12
        , width (px 800)
        , height (px 600)
        ]
        [ Element.html (chart model) ]


chart : Model -> Svg msg
chart model =
    case model.statistics of
        Nothing ->
            LineChart.view .x .y [ LineChart.line Colors.red Dots.none "Data" model.data ]

        Just stats ->
            LineChart.viewCustom (defaults (Display.label "x" model.xLabel) (Display.label "y" model.yLabel))
                [ LineChart.line Colors.red Dots.none "Data" model.data
                , LineChart.line Colors.blue Dots.none "Regression" [ stats.leftRegressionPoint, stats.rightRegressionPoint ]
                ]


defaults xLabel_ yLabel_ =
    { x = Axis.default 700 xLabel_ .x
    , y = Axis.default 400 yLabel_ .y
    , container = Container.default "line-chart-1"
    , interpolation = Interpolation.default
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.default
    , line = Line.wider 1.875
    , dots = Dots.default
    }



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
        , height (px 515)
        , paddingXY 8 12
        , moveDown 25
        ]
        [ el []
            (text <| numberOfRecordsString model.csvData)
        , el []
            (text <| plotTypeAsString model.plotType)
        , showIfNot (model.csvData == Nothing) <| xInfoDisplay model
        , showIfNot (model.csvData == Nothing) <| Display.info "y" model.yLabel .y model.data
        , showIfNot (model.csvData == Nothing) <| Display.correlationInfo model.data
        , showIfNot (model.csvData == Nothing) <| inputXMin model
        , showIfNot (model.csvData == Nothing) <| inputXMax model
        , showIfNot (model.csvData == Nothing) <| recomputeButton
        , showIfNot (model.csvData == Nothing) <| resetButton
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
        TimeSeries ->
            Display.smallInfo "x" model.xLabel .x model.data

        ScatterPlot ->
            Display.info "x" model.xLabel .x model.data


plotTypeAsString : PlotType -> String
plotTypeAsString plotType =
    case plotType of
        TimeSeries ->
            "Plot: time series"

        ScatterPlot ->
            "Plot: scatter"



--
-- RAW DATA DISPLAY
--


numberOfRecordsString : Maybe Csv -> String
numberOfRecordsString maybeCsvData =
    case maybeCsvData of
        Nothing ->
            "No records yet"

        Just data ->
            "Records: " ++ String.fromInt (List.length data.records)


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
                        Element.text <| Display.stringOfFloat point.x
              }
            , { header = Element.text (Display.label "y" model.yLabel)
              , width = fill
              , view =
                    \point ->
                        Element.text <| Display.stringOfFloat point.y
              }
            ]
        }



--
-- INPUT FIELDS
--


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
            { onPress = Just CsvRequested
            , label = el [] (text "Open CSV file")
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
