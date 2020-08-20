module Chart exposing
    ( Chart, Graph, GraphType(..)
    , boundingBox, emptyGraph, graph, lineGraph, scatter, errorBars, setConfidence
    , view, chart, addGraph, addGraphIf
    )

{-| Functions for building graphs A chart consists of one more graphs.


## Types

@docs Chart, Graph, GraphType


## Constructing and operating on graphs

@docs boundingBox, emptyGraph, graph, lineGraph, scatter, errorBars, setConfidence


## Constructing and ope rating on charts

@docs view, chart, addGraph, addGraphIf

-}

import Axis
import Color
import Data exposing (xCoord, yCoord)
import ErrorBars exposing (ErrorBar)
import Path exposing (Path)
import RawData
import SampleData
import Scale exposing (ContinuousScale)
import Shape
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))
import Utility


{-| -}
type alias Chart =
    { boundingBox : BoundingBox
    , confidence : Maybe Float
    , data : List Graph
    }


{-| -}
type GraphType
    = Line
    | Scatter
    | MeanLine


{-| -}
type alias BoundingBox =
    { xMin : Float
    , xMax : Float
    , yMin : Float
    , yMax : Float
    }


{-| -}
type alias Graph =
    { graphType : GraphType
    , r : Float
    , g : Float
    , b : Float
    , boundingBox : BoundingBox
    , data : Data
    }


{-| -}
type alias Data =
    List ( Float, Float )


{-| -}
emptyGraph : Graph
emptyGraph =
    { graphType = Line
    , r = 0
    , g = 0
    , b = 0
    , boundingBox = boundingBox []
    , data = []
    }


{-| -}
boundingBox : Data.Data -> BoundingBox
boundingBox data =
    { xMin = RawData.minimum xCoord data |> Maybe.withDefault 0
    , xMax = RawData.maximum xCoord data |> Maybe.withDefault 1
    , yMin = RawData.minimum yCoord data |> Maybe.withDefault 0
    , yMax = RawData.maximum yCoord data |> Maybe.withDefault 1
    }


{-| -}
graph : GraphType -> Float -> Float -> Float -> Data.Data -> Graph
graph graphType r g b data =
    { graphType = graphType
    , r = r
    , g = g
    , b = b
    , boundingBox = boundingBox data
    , data = data
    }


{-| -}
setConfidence : Maybe Float -> Chart -> Chart
setConfidence conf chart_ =
    { chart_ | confidence = conf }


{-| -}
chart : Graph -> Chart
chart g =
    { boundingBox = g.boundingBox
    , confidence = Nothing
    , data = [ g ]
    }


{-| -}
addGraph : Graph -> Chart -> Chart
addGraph newGraph c =
    let
        adjustedGraph =
            { newGraph | boundingBox = c.boundingBox }
    in
    { c | data = adjustedGraph :: c.data }


{-| -}
addGraphIf : Bool -> Graph -> Chart -> Chart
addGraphIf flag newGraph c =
    case flag of
        True ->
            addGraph newGraph c

        False ->
            c


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : Float -> Float -> ContinuousScale Float
xScale xMin xMax =
    Scale.linear ( w - 2 * padding, 0 ) ( xMax, xMin )


yScale : Float -> Float -> ContinuousScale Float
yScale yMin yMax =
    Scale.linear ( h - 2 * padding, 0 ) ( yMin, yMax )


xAxis : BoundingBox -> Svg msg
xAxis bb =
    Axis.bottom [ Axis.tickCount 5 ] (xScale bb.xMin bb.xMax)


yAxis : BoundingBox -> Svg msg
yAxis bb =
    Axis.left [ Axis.tickCount 5 ] (yScale bb.yMin bb.yMax)


transformScale : BoundingBox -> ( Float, Float ) -> Maybe ( Float, Float )
transformScale bb ( x, y ) =
    Just ( Scale.convert (xScale bb.xMin bb.xMax) x, Scale.convert (yScale bb.yMin bb.yMax) y )


line : Graph -> Path
line g =
    List.map (transformScale g.boundingBox) g.data
        |> Shape.line Shape.monotoneInXCurve


{-| Draw a line with given colors and bounding box
-}
basicLine : Float -> Float -> Float -> BoundingBox -> Data -> Svg msg
basicLine r g b bb dat =
    List.map (transformScale bb) dat
        |> Shape.line Shape.monotoneInXCurve
        |> (\path -> Path.element path [ stroke (Paint (Color.rgb r g b)), strokeWidth 2.0, fill PaintNone ])


{-| Draw a scatter plot
-}
scatter : Graph -> Svg msg
scatter gr =
    gr.data
        |> List.map (transformScale gr.boundingBox)
        |> Utility.maybeValues
        |> List.map (\( x, y ) -> circle [ cx x, cy y, r 2.5, fill (Paint (Color.rgb gr.r gr.g gr.b)) ] [])
        |> g []


{-| Draw a broken line graph
-}
lineGraph : Graph -> Svg msg
lineGraph g =
    Path.element (line g) [ stroke (Paint (Color.rgb g.r g.g g.b)), strokeWidth 1.5, fill PaintNone ]


{-| Draw error bars with given confidence level for the given data
-}
errorBars : Float -> Data -> Svg msg
errorBars confidenceLevel data_ =
    let
        bb =
            boundingBox data_

        ebList =
            ErrorBars.normal confidenceLevel data_

        meanValues =
            List.map (\eb -> ( eb.x, eb.y )) ebList

        ebList2 =
            List.map (\eb -> [ ( eb.x, eb.bottom ), ( eb.x, eb.top ) ]) ebList

        errorBarGraph =
            List.map (basicLine 0 0 1 bb) ebList2
    in
    g [] errorBarGraph


{-| Draw the xxx
-}
meanLine : Graph -> Svg msg
meanLine gr =
    let
        bb =
            gr.boundingBox

        ebList =
            ErrorBars.normal 1 gr.data

        meanValues =
            List.map (\eb -> ( eb.x, eb.y )) ebList

        meanValueGraph =
            lineGraph { gr | data = meanValues, r = 1, g = 0, b = 0 }
    in
    meanValueGraph


{-| Convert a graph to SVG
-}
viewGraph : Maybe Float -> Graph -> Svg msg
viewGraph confidence g =
    case g.graphType of
        Line ->
            lineGraph g

        Scatter ->
            scatter g

        MeanLine ->
            meanLine g


{-| Convert a chart to SVG
-}
view : Maybe (Svg msg) -> Chart -> Svg msg
view annotation_ chartData =
    let
        svgList =
            List.map (viewGraph chartData.confidence) chartData.data

        finalSvgList =
            case annotation_ of
                Nothing ->
                    svgList

                Just annotation ->
                    annotation :: svgList
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis chartData.boundingBox ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis chartData.boundingBox ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            finalSvgList
        ]
