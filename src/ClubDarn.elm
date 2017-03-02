module ClubDarn exposing (..)

import Route exposing (Route)
import Model exposing (Model)
import Msg exposing (Msg)
import Update exposing (update)
import View exposing (view)
import Html exposing (..)
import Navigation exposing (Location)
import UrlParser exposing (..)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            Route.parseLocation location
    in
        { name = "world"
        , route = route
        }
            ! []


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
