module View exposing (..)

import Html exposing (..)
import Msg exposing (Msg)
import Model exposing (Model)
import Route exposing (Route)


view : Model -> Html Msg
view model =
    div []
        [ case model.route of
            Route.SongSearch query ->
                text ("Song search " ++ (Maybe.withDefault "" query))

            Route.ArtistSearch query ->
                text ("Artist search " ++ (Maybe.withDefault "" query))

            Route.SeriesSearch query ->
                text ("Series search " ++ (Maybe.withDefault "" query))

            _ ->
                text "Other"
        ]
