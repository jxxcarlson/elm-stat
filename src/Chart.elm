module Chart exposing
    ( BoundingBox, Format, Graph, GraphType(..), Chart
    , boundingBox, emptyGraph, graph, lineGraph, meanLine, scatter, errorBars
    , view, chart, addGraph
    )

{-| Functions for building graphs and charts. A chart consists of one more graphs.
Graphs come in various flavors, notably line and scatter.

In this module, `Svg` means `TypedSvg.Core.Svg`


## Types

@docs BoundingBox, Format, Graph, GraphType, Chart


## Constructing and operating on graphs

@docs boundingBox, emptyGraph, graph, lineGraph, meanLine, scatter, errorBars


## Constructing and operating on charts

@docs view, chart, addGraph

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


{-| A chart consists of a boundig box and a list of Graphs.
A chart is rendered to SVG by the `view` function.
-}
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


{-| A box containing data of the form `List (Float, Float)`
-}
type alias BoundingBox =
    { xMin : Float
    , xMax : Float
    , yMin : Float
    , yMax : Float
    }


{-| A Graph consists of data in the form of `List (Float, Float)`,
a bounding box, a choice of color, and a GraphType, e.g, `Line` or `Scatter`.
-}
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


{-| Format determines the height, width, and appearance of the rendered chart
-}
type alias Format =
    { width : Float
    , height : Float
    , padding : Float
    }


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


{-| Compute a bounding box from data, where

    type alias Data =
        List ( Float, Float )

-}
boundingBox : Data.Data -> BoundingBox
boundingBox data =
    { xMin = RawData.minimum xCoord data |> Maybe.withDefault 0
    , xMax = RawData.maximum xCoord data |> Maybe.withDefault 1
    , yMin = RawData.minimum yCoord data |> Maybe.withDefault 0
    , yMax = RawData.maximum yCoord data |> Maybe.withDefault 1
    }


{-| Make a graph of given GraphType and given color r, g, b from the given data.
where

    type alias Data =
        List ( Float, Float )

-}
graph : GraphType -> Float -> Float -> Float -> Data.Data -> Graph
graph graphType r g b data =
    { graphType = graphType
    , r = r
    , g = g
    , b = b
    , boundingBox = boundingBox data
    , data = data
    }


{-| Make a chart out of a graph
-}
chart : Graph -> Chart
chart g =
    { boundingBox = g.boundingBox
    , confidence = Nothing
    , data = [ g ]
    }


{-| Add a graph to an existing Chart
-}
addGraph : Graph -> Chart -> Chart
addGraph newGraph c =
    let
        adjustedGraph =
            { newGraph | boundingBox = c.boundingBox }
    in
    { c | data = adjustedGraph :: c.data }


xScale : Format -> Float -> Float -> ContinuousScale Float
xScale format xMin xMax =
    Scale.linear ( format.width - 2 * format.padding, 0 ) ( xMax, xMin )


yScale : Format -> Float -> Float -> ContinuousScale Float
yScale format yMin yMax =
    Scale.linear ( format.height - 2 * format.padding, 0 ) ( yMin, yMax )


xAxis : Format -> BoundingBox -> Svg msg
xAxis format bb =
    Axis.bottom [ Axis.tickCount 5 ] (xScale format bb.xMin bb.xMax)


yAxis : Format -> BoundingBox -> Svg msg
yAxis format bb =
    Axis.left [ Axis.tickCount 5 ] (yScale format bb.yMin bb.yMax)


transformScale : Format -> BoundingBox -> ( Float, Float ) -> Maybe ( Float, Float )
transformScale format bb ( x, y ) =
    Just ( Scale.convert (xScale format bb.xMin bb.xMax) x, Scale.convert (yScale format bb.yMin bb.yMax) y )


line : Format -> Graph -> Path
line format g =
    List.map (transformScale format g.boundingBox) g.data
        |> Shape.line Shape.monotoneInXCurve


{-| Draw a line with given colors and bounding box
-}
basicLine : Format -> Float -> Float -> Float -> BoundingBox -> Data -> Svg msg
basicLine format r g b bb dat =
    List.map (transformScale format bb) dat
        |> Shape.line Shape.monotoneInXCurve
        |> (\path -> Path.element path [ stroke (Paint (Color.rgb r g b)), strokeWidth 2.0, fill PaintNone ])


{-| Render graph data to SVG as a scatter plot.
-}
scatter : Format -> Graph -> Svg msg
scatter format gr =
    gr.data
        |> List.map (transformScale format gr.boundingBox)
        |> Utility.maybeValues
        |> List.map (\( x, y ) -> circle [ cx x, cy y, r 2.5, fill (Paint (Color.rgb gr.r gr.g gr.b)) ] [])
        |> g []


{-| Render graph data to SVG as a broken line.
-}
lineGraph : Format -> Graph -> Svg msg
lineGraph format g =
    Path.element (line format g) [ stroke (Paint (Color.rgb g.r g.g g.b)), strokeWidth 1.5, fill PaintNone ]


{-| Render error bars with given confidence level for the given data as SVG
-}
errorBars : Format -> Float -> Data -> Svg msg
errorBars format confidenceLevel data_ =
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
            List.map (basicLine format 0 0 1 bb) ebList2
    in
    g [] errorBarGraph


{-| Draw a line through mean values of the data.
-}
meanLine : Format -> Graph -> Svg msg
meanLine format gr =
    let
        bb =
            gr.boundingBox

        ebList =
            ErrorBars.normal 1 gr.data

        meanValues =
            List.map (\eb -> ( eb.x, eb.y )) ebList

        meanValueGraph =
            lineGraph format { gr | data = meanValues, r = 1, g = 0, b = 0 }
    in
    meanValueGraph


{-| Convert a graph to SVG
-}
viewGraph : Format -> Maybe Float -> Graph -> Svg msg
viewGraph format confidence g =
    case g.graphType of
        Line ->
            lineGraph format g

        Scatter ->
            scatter format g

        MeanLine ->
            meanLine format g


{-| Convert a chart to (typed) SVG. Parameters:

  - `Format`: determines the dimensions and appearance of the output

  - `Maybe (Svg mg)`: this is an "annotation," which maybe`Nothing`or`Just svgValue`, where`svgValue`is any value of type`Svg msg`.
    Here`Svg`meand `TypedSvg.Core.Svg`.

  - `Chart`: the chart to be rendered

One use of annotations is to add error bars to a chart. See
`./examples/src/Hubble.elm`

-}
view : Format -> Maybe (Svg msg) -> Chart -> Svg msg
view format annotation_ chartData =
    let
        svgList =
            List.map (viewGraph format chartData.confidence) chartData.data

        finalSvgList =
            case annotation_ of
                Nothing ->
                    svgList

                Just annotation ->
                    annotation :: svgList
    in
    svg [ viewBox 0 0 format.width format.height ]
        [ g [ transform [ Translate (format.padding - 1) (format.height - format.padding) ] ]
            [ xAxis format chartData.boundingBox ]
        , g [ transform [ Translate (format.padding - 1) format.padding ] ]
            [ yAxis format chartData.boundingBox ]
        , g [ transform [ Translate format.padding format.padding ], class [ "series" ] ]
            finalSvgList
        ]
