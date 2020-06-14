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


histogram : List Float -> List (Bin Float Float)
histogram model =
    Histogram.custom (Histogram.steps (Histogram.binCount ( 0, 4 ) 20)) identity
        |> Histogram.withDomain ( 0, 4 )
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


xScale : ContinuousScale Float
xScale =
    Scale.linear ( 0, w - 2 * padding ) ( 0, 4 )


yScaleFromBins : List (Bin Float Float) -> ContinuousScale Float
yScaleFromBins bins =
    List.map .length bins
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat
        |> Tuple.pair 0
        |> Scale.linear ( h - 2 * padding, 0 )


xAxis : List Float -> Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount 20 ] xScale


yAxis : List (Bin Float Float) -> Svg msg
yAxis bins =
    Axis.left [ Axis.tickCount 10 ] (yScaleFromBins bins)


column : ContinuousScale Float -> Bin Float Float -> Svg msg
column yScale { length, x0, x1 } =
    rect
        [ x <| Scale.convert xScale x0
        , y <| Scale.convert yScale (toFloat length)
        , width <| (Scale.convert xScale x1 - Scale.convert xScale x0) - 2
        , height <| h - Scale.convert yScale (toFloat length) - 2 * padding
        , fill <| Paint <| Color.rgb255 46 118 149
        ]
        []


view model =
    let
        bins =
            histogram model
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis bins ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column (yScaleFromBins bins)) bins
        ]
