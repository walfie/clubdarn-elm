module ClubDarn.Msg exposing (..)

import Navigation exposing (Location)
import ClubDarn.Route as Route
import ClubDarn.Model as Model
import Http
import Material


type Msg
    = QueryInput String
    | ChangeSearchType Route.SearchType
    | QuerySubmit
    | ApiResult String (Result Http.Error Model.PaginatedItems)
    | LocationChange Location
    | RetryRequest
    | SelectTab Int
    | ShowSong (Maybe Model.Song)
    | UpdateSettings Model.Settings
    | Mdl (Material.Msg Msg)
