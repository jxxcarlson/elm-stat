module HistogramChart exposing (histogram, view)

import Axis
import Color
import Histogram exposing (Bin, HistogramGenerator)
import Random exposing (Generator, Seed)
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (class, fill, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


histogram : List Float -> ( Float, Float ) -> Int -> List (Bin Float Float)
histogram model range binNum =
    Histogram.custom (Histogram.steps (Histogram.binCount range binNum)) identity
        |> Histogram.withDomain range
        |> Histogram.compute model


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : ( Float, Float ) -> ContinuousScale Float
xScale range =
    Scale.linear ( 0, w - 2 * padding ) range


yScaleFromBins : List (Bin Float Float) -> ContinuousScale Float
yScaleFromBins bins =
    List.map .length bins
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat
        |> Tuple.pair 0
        |> Scale.linear ( h - 2 * padding, 0 )


xAxis : ContinuousScale Float -> Int -> Svg msg
xAxis xscale tc =
    Axis.bottom [ Axis.tickCount tc ] xscale


yAxis : List (Bin Float Float) -> Svg msg
yAxis bins =
    Axis.left [ Axis.tickCount 10 ] (yScaleFromBins bins)


column : ContinuousScale Float -> ContinuousScale Float -> Bin Float Float -> Svg msg
column yScale xscale { length, x0, x1 } =
    rect
        [ x <| Scale.convert xscale x0
        , y <| Scale.convert yScale (toFloat length)
        , width <| (Scale.convert xscale x1 - Scale.convert xscale x0) - 2
        , height <| h - Scale.convert yScale (toFloat length) - 2 * padding
        , fill <| Paint <| Color.rgb255 117 165 255
        ]
        []


view model range binNum xnum =
    let
        bins =
            histogram model range binNum

        xscale =
            xScale range
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis xscale xnum ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis bins ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column (yScaleFromBins bins) xscale) bins
        ]
