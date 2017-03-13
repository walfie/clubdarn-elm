module ClubDarn.Main exposing (..)

import ClubDarn.Route as Route exposing (Route)
import ClubDarn.Model as Model exposing (Model)
import ClubDarn.Msg as Msg exposing (Msg)
import ClubDarn.Update as Update exposing (update, saveSettings)
import ClubDarn.View exposing (view)
import Html exposing (..)
import Navigation exposing (Location)
import UrlParser exposing (..)
import RemoteData
import LruCache exposing (LruCache)
import Material


defaultSettings : Model.Settings
defaultSettings =
    { serialNo = Nothing }


initialModel : Maybe Model.Settings -> Location -> Model
initialModel maybeSettings location =
    { query = ""
    , searchType = Route.SongSearch
    , items = RemoteData.NotAsked
    , route = Route.parseLocation location
    , activeSong = Nothing
    , fileSearchState = Nothing
    , responseCache = LruCache.empty 50
    , settings = Maybe.withDefault defaultSettings maybeSettings
    , mdl = Material.model
    }


init : Maybe Model.Settings -> Location -> ( Model, Cmd Msg )
init maybeSettings location =
    let
        ( updatedModel, cmd ) =
            initialModel maybeSettings location
                |> update (Msg.LocationChange location)
    in
        updatedModel ! [ cmd, Material.init Msg.Mdl ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Msg.Mdl model
        , Update.fileMetadata Msg.ReceiveFileMetadata
        ]


main : Program (Maybe Model.Settings) Model Msg
main =
    Navigation.programWithFlags Msg.LocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
