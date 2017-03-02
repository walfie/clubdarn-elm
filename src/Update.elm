module Update exposing (..)

import Msg exposing (Msg(..))
import Model exposing (Model)
import Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange location ->
            { model | route = (Route.parseLocation location) } ! []

        NoOp ->
            model ! []
