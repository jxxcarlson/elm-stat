module Style exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font


--
-- STYLE
--


outer =
    [ Background.color (rgb255 180 180 180)
    , height fill
    , width fill
    ]


mainColumn =
    [ Background.color (rgb255 180 180 180)
    , paddingXY 20 20
    , height fill
    , width fill
    ]


footer =
    [ Background.color (rgb255 40 40 40)
    , Font.color (rgb 250 250 250)
    , Font.size 14
    , paddingXY 20 5
    , height (px 30)
    , width fill
    , spacing 18
    ]


link =
    [ Font.color (rgb 100 100 240)
    ]


button =
    [ buttonBackground
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    , Font.size 14
    , centerX
    ]


plainButton =
    [ Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    , Font.size 14
    , centerX
    ]


buttonBackground =
    Background.color (rgb255 100 100 100)


buttonActiveBackground =
    Background.color (rgb255 20 20 130)


table =
    [ Background.color (rgb255 245 245 245)
    , width <| px 200
    , height <| px 560
    , scrollbarY
    , Font.size 12
    , paddingXY 8 12
    , alignTop
    ]
