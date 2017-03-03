module ClubDarn.Model exposing (..)

import ClubDarn.Route exposing (Route)


type alias Model =
    { query : String
    , route : Route
    }
