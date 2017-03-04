module ClubDarn.Msg exposing (..)

import Navigation exposing (Location)
import ClubDarn.Route as Route


type Msg
    = QueryInput String
    | ChangeSearchType Route.SearchType
    | QuerySubmit
    | LocationChange Location
