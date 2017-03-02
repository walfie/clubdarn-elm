module ClubDarn.Model exposing (..)

import ClubDarn.Route exposing (Route)


type alias Model =
    { name : String
    , route : Route
    }
