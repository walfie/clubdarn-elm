module ClubDarn.Main exposing (..)

import ClubDarn.Route as Route exposing (Route)
import ClubDarn.Model as Model exposing (Model)
import ClubDarn.Msg as Msg exposing (Msg)
import ClubDarn.Update as Update exposing (update, saveSettings)
import ClubDarn.View exposing (view)
import Navigation exposing (Location)
import RemoteData
import LruCache exposing (LruCache)
import Material


defaultSettings : Model.Settings
defaultSettings =
    { serialNo = Nothing }


initialModel : Model.Flags -> Location -> Model
initialModel flags location =
    { query = ""
    , searchType = Route.SongSearch
    , items = RemoteData.NotAsked
    , route = Route.parseLocation location
    , activeSong = Nothing
    , fileSearchState = Nothing
    , searchTabTarget = Route.MainSearch
    , responseCache = LruCache.empty 50
    , apiBaseUrl = flags.apiBaseUrl
    , settings = Maybe.withDefault defaultSettings flags.settings
    , mdl = Material.model
    }


init : Model.Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        ( updatedModel, cmd ) =
            initialModel flags location
                |> update (Msg.LocationChange location)
    in
        updatedModel ! [ cmd, Material.init Msg.Mdl ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Msg.Mdl model
        , Update.fileMetadata Msg.ReceiveFileMetadata
        ]


main : Program Model.Flags Model Msg
main =
    Navigation.programWithFlags Msg.LocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
