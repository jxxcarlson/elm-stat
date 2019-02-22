module Utility exposing (listGetAt, maybeValues, maybeCombine, maybeJoin, toggleElement)

{- Credits; from elm-community/list-extra. Copied
   here to eliminate a dependency.
-}


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
