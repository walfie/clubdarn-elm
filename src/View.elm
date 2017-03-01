module View exposing (..)

import Html exposing (..)
import Msg exposing (Msg)
import Model exposing (Model)


view : Model -> Html Msg
view model =
    div []
        [ text ("Hello " ++ model.name ++ "!") ]
