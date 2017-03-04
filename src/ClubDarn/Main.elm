module ClubDarn.Main exposing (..)

import ClubDarn.Route as Route exposing (Route)
import ClubDarn.Model as Model exposing (Model)
import ClubDarn.Msg as Msg exposing (Msg)
import ClubDarn.Update exposing (update)
import ClubDarn.View exposing (view)
import Html exposing (..)
import Navigation exposing (Location)
import UrlParser exposing (..)
import RemoteData


init : Location -> ( Model, Cmd Msg )
init location =
    { query = ""
    , searchType = Route.SongSearch
    , items = RemoteData.NotAsked
    , route = Route.parseLocation location
    }
        |> update (Msg.LocationChange location)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Navigation.program Msg.LocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
