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
            let
                route =
                    Route.parseLocation location
            in
                { model | route = route, query = "" } ! []

        QueryInput query ->
            { model | query = query } ! []

        ChangeSearchType searchType ->
            { model | searchType = searchType } ! []

        QuerySubmit ->
            let
                query =
                    if (String.isEmpty model.query) then
                        Nothing
                    else
                        (Just model.query)

                newRoute =
                    Route.SearchResults model.searchType query
            in
                model ! [ newRoute |> Route.reverse |> Navigation.newUrl ]
