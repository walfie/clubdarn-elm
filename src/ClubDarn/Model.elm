module ClubDarn.Model exposing (..)

import ClubDarn.Route as Route exposing (Route)


type alias Model =
    { query : String
    , searchType : Route.SearchType
    , route : Route
    }
