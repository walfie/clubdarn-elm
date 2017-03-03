module ClubDarn.Update exposing (..)

import ClubDarn.Msg exposing (Msg(..))
import ClubDarn.Model exposing (Model)
import ClubDarn.Route as Route exposing (Route)
import Navigation
import Task


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange location ->
            { model | route = (Route.parseLocation location), query = "" } ! []

        QueryInput query ->
            { model | query = query } ! []

        QuerySubmit ->
            let
                query =
                    if (String.isEmpty model.query) then
                        Nothing
                    else
                        (Just model.query)
            in
                model ! [ searchUrl model.route query |> Navigation.newUrl ]


searchUrl : Route -> Maybe String -> String
searchUrl route query =
    case route of
        Route.SongSearch _ ->
            Route.reverse (Route.SongSearch query)

        Route.ArtistSearch _ ->
            Route.reverse (Route.ArtistSearch query)

        Route.SeriesSearch _ ->
            Route.reverse (Route.SeriesSearch query)

        _ ->
            Route.reverse (Route.SongSearch query)
