module ClubDarn.Util exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)


reverseSortBy : (a -> comparable) -> List a -> List a
reverseSortBy f =
    List.sortWith (reverse f)


reverse : (a -> comparable) -> a -> a -> Order
reverse f x y =
    case compare (f x) (f y) of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


{-| Taken from krisajenkins/elm-exts
-}
groupBy : (v -> comparable) -> List v -> Dict comparable (List v)
groupBy f =
    let
        reducer g x d =
            let
                key =
                    g x

                newValue =
                    x :: Maybe.withDefault [] (Dict.get key d)
            in
                Dict.insert key newValue d
    in
        List.foldl (reducer f) Dict.empty


maybeFold : (a -> b) -> b -> Maybe a -> b
maybeFold f default opt =
    opt
        |> Maybe.map f
        |> Maybe.withDefault default


orEmptyString : Maybe String -> String
orEmptyString =
    Maybe.withDefault ""


orEmptyText : Maybe String -> Html a
orEmptyText maybe =
    maybe |> Maybe.withDefault "" |> Html.text


maybeToList : Maybe a -> List a
maybeToList maybe =
    case maybe of
        Just x ->
            [ x ]

        Nothing ->
            []


flattenListOfMaybe : List (Maybe a) -> List a
flattenListOfMaybe =
    List.concatMap maybeToList
