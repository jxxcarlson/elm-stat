module DataParser exposing (data, field, record)

import Parser as P exposing ((|.), (|=), Parser, chompUntil, chompWhile, getChompedString, run, spaces, succeed, symbol)
import Parser.Extras exposing (many)


type alias Record =
    List String


type alias Data =
    List Record


{-| Parse strings, discarding preceding white space,
and stopping at white space.
-}
field : Parser String
field =
    (getChompedString <|
        succeed ()
            |. spaces
            |. chompUntil " "
    )
        |> P.map String.trim


adjoinLastField : Record -> Parser Record
adjoinLastField items =
    (getChompedString <|
        succeed ()
            |. chompWhile (\c -> c == ' ')
            |. chompUntil "\n"
            |. symbol "\n"
    )
        |> P.map (\str -> items ++ [ String.trim str ])


record =
    many field |> P.andThen (\records -> adjoinLastField records)


data =
    many record



-- |= lastField
