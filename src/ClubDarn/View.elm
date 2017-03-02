module ClubDarn.View exposing (..)

import Html exposing (..)
import ClubDarn.Msg exposing (Msg)
import ClubDarn.Model exposing (Model)
import ClubDarn.Route as Route exposing (Route)


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
