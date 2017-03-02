module ClubDarn.Update exposing (..)

import ClubDarn.Msg exposing (Msg(..))
import ClubDarn.Model exposing (Model)
import ClubDarn.Route as Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange location ->
            { model | route = (Route.parseLocation location) } ! []

        NoOp ->
            model ! []
