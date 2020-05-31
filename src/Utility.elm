module Utility exposing (FrequencyTable, addToTable, buildTable, combineTuple, listGetAt, maybeCombine, maybeJoin, maybeValues, substractTuple, toggleElement)

import Dict exposing (Dict)



{- Credits; from elm-community/list-extra. Copied
   here to eliminate a dependency.
-}


type alias FrequencyTable comparable =
    Dict comparable Int


listGetAt : Int -> List a -> Maybe a
listGetAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs



{- Credits; from elm-community/maybe-extra. Copied
   here to eliminate a dependency.
-}


maybeJoin : Maybe (Maybe a) -> Maybe a
maybeJoin mx =
    case mx of
        Just x ->
            x

        Nothing ->
            Nothing


{-| Convert a list of `Maybe a` to a list of `a` only for the values different
from `Nothing`.
values [ Just 1, Nothing, Just 2 ] == [ 1, 2 ]
-}
maybeValues : List (Maybe a) -> List a
maybeValues =
    List.foldr foldrValues []


foldrValues : Maybe a -> List a -> List a
foldrValues item list =
    case item of
        Nothing ->
            list

        Just v ->
            v :: list


maybeCombine : List (Maybe a) -> Maybe (List a)
maybeCombine =
    traverse identity


traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f =
    let
        step e acc =
            case f e of
                Nothing ->
                    Nothing

                Just x ->
                    Maybe.map ((::) x) acc
    in
    List.foldr step (Just [])


{-| Remove the first occurrence of a value from a list.
(From List.Extra)
-}
removeFromList : a -> List a -> List a
removeFromList x xs =
    case xs of
        [] ->
            []

        y :: ys ->
            if x == y then
                ys

            else
                y :: removeFromList x ys


toggleElement : a -> List a -> List a
toggleElement element list =
    case List.member element list of
        True ->
            removeFromList element list

        False ->
            element :: list


{-| buildTable [1,2,3,3,1,1,2,1] == Dict.fromList [(1,4),(2,2),(3,2)]
-}
buildTable : List comparable -> FrequencyTable comparable
buildTable list =
    list |> List.foldl (\item dict -> addToTable item dict) Dict.empty


combineTuple : ( Maybe a, Maybe b ) -> Maybe ( a, b )
combineTuple ( aa, bb ) =
    case ( aa, bb ) of
        ( Just k, Just v ) ->
            Just ( k, v )

        ( _, _ ) ->
            Nothing


substractTuple : ( number, number ) -> ( number, number ) -> ( number, number )
substractTuple x y =
    ( Tuple.first x - Tuple.first y, Tuple.second x - Tuple.second y )


addToTable : comparable -> FrequencyTable comparable -> FrequencyTable comparable
addToTable item dict =
    case Dict.get item dict of
        Nothing ->
            Dict.insert item 1 dict

        Just f ->
            Dict.insert item (f + 1) dict
