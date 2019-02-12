module DataParser exposing
    ( Data
    , Record
    , string
    )

import Parser exposing (..)
import Parser.Extras exposing (many)


type alias Record =
    List String


type alias Data =
    List Record


string : Parser String
string =
    (succeed String.slice
        |= getOffset
        |. chompWhile (\c -> c == ' ')
        |. chompWhile (\c -> c /= ' ' && c /= '\n')
        |. chompWhile (\c -> c == ' ')
        |= getOffset
        |= getSource
    )
        |> map String.trim
