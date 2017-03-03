module ClubDarn.Msg exposing (..)

import Navigation exposing (Location)


type Msg
    = QueryInput String
    | QuerySubmit
    | LocationChange Location
