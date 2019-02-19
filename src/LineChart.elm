module LineChart exposing (Chart, Graph, view)

{-| This module shows how to build a simple line and area chart using some of
the primitives provided in this library.
-}

import Axis
import Color
import Path exposing (Path)
import SampleData exposing (simpleData)
import Scale exposing (ContinuousScale)
import Shape
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))


type alias Chart =
    { boundingBox : BoundingBox
    , data : List Graph
    }


type alias BoundingBox =
    { xMin : Float
    , xMax : Float
    , yMin : Float
    , yMax : Float
    }


type alias Graph =
    { r : Float
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


transformToLineData : BoundingBox -> ( Float, Float ) -> Maybe ( Float, Float )
transformToLineData boundingBox ( x, y ) =
    Just ( Scale.convert (xScale boundingBox.xMin boundingBox.xMax) x, Scale.convert (yScale boundingBox.yMin boundingBox.yMax) y )


line : Graph -> Path
line gd =
    List.map (transformToLineData gd.boundingBox) gd.data
        |> Shape.line Shape.monotoneInXCurve


viewGraph : Graph -> Svg msg
viewGraph gd =
    Path.element (line gd) [ stroke (Color.rgb gd.r gd.g gd.b), strokeWidth 1, fill FillNone ]


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
