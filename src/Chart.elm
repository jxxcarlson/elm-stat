module Chart exposing (Chart, Graph, GraphType(..), graph, addGraph, chart, scatter, view)

{-| This module shows how to build a simple line and area chart using some of
the primitives provided in this library.
-}

import Axis
import Color
import Path exposing (Path)
import SampleData exposing (simpleData)
import Scale exposing (ContinuousScale)
import Shape
import TypedSvg exposing (g, svg, circle)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth, cx, cy, r)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))
import Data exposing (xCoord, yCoord)
import Stat
import Utility


type alias Chart =
    { boundingBox : BoundingBox
    , data : List Graph
    }


type GraphType
    = Line
    | Scatter


type alias BoundingBox =
    { xMin : Float
    , xMax : Float
    , yMin : Float
    , yMax : Float
    }


boundingBox : Data.Data -> BoundingBox
boundingBox data =
    { xMin = Stat.minimum xCoord data |> Maybe.withDefault 0
    , xMax = Stat.maximum xCoord data |> Maybe.withDefault 1
    , yMin = Stat.minimum yCoord data |> Maybe.withDefault 0
    , yMax = Stat.maximum yCoord data |> Maybe.withDefault 1
    }


graph : GraphType -> Float -> Float -> Float -> Data.Data -> Graph
graph graphType r g b data =
    { graphType = graphType
    , r = r
    , g = g
    , b = b
    , boundingBox = boundingBox data
    , data = data
    }


chart : Graph -> Chart
chart g =
    { boundingBox = g.boundingBox
    , data = [ g ]
    }


addGraph : Graph -> Chart -> Chart
addGraph newGraph c =
    let
        adjustedGraph =
            { newGraph | boundingBox = c.boundingBox }
    in
        { c | data = adjustedGraph :: c.data }


type alias Graph =
    { graphType : GraphType
    , r : Float
    , g : Float
    , b : Float
    , boundingBox : BoundingBox
    , data : Data
    }


type alias Data =
    List ( Float, Float )


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


scatter : Graph -> Svg msg
scatter gr =
    gr.data
        |> List.map (transformScale gr.boundingBox)
        |> Utility.maybeValues
        |> List.map (\( x, y ) -> circle [ cx x, cy y, r 2.5, fill (Fill (Color.rgb gr.r gr.g gr.b)) ] [])
        |> g []


viewGraph : Graph -> Svg msg
viewGraph g =
    case g.graphType of
        Line ->
            Path.element (line g) [ stroke (Color.rgb g.r g.g g.b), strokeWidth 1.5, fill FillNone ]

        Scatter ->
            scatter g


view : Chart -> Svg msg
view chartData =
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis chartData.boundingBox ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis chartData.boundingBox ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            (List.map viewGraph chartData.data)
        ]
