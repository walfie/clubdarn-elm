module ClubDarn exposing (..)

import Model exposing (Model)
import Msg exposing (Msg)
import Update exposing (update)
import View exposing (view)
import Html exposing (..)


init : ( Model, Cmd Msg )
init =
    { name = "world" } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
