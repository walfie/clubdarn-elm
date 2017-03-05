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
import LruCache exposing (LruCache)
import Material


initialModel : Location -> Model
initialModel location =
    { query = ""
    , searchType = Route.SongSearch
    , items = RemoteData.NotAsked
    , route = Route.parseLocation location
    , responseCache = LruCache.empty 50
    , mdl = Material.model
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( updatedModel, cmd ) =
            initialModel location |> update (Msg.LocationChange location)
    in
        updatedModel ! [ cmd, Material.init Msg.Mdl ]


main : Program Never Model Msg
main =
    Navigation.program Msg.LocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = Material.subscriptions Msg.Mdl
        }
