module ClubDarn.Msg exposing (..)

import Navigation exposing (Location)
import ClubDarn.Route as Route
import ClubDarn.Model as Model
import Http


type Msg
    = QueryInput String
    | ChangeSearchType Route.SearchType
    | QuerySubmit
    | ApiResult (Result Http.Error Model.PaginatedItems)
    | LocationChange Location
