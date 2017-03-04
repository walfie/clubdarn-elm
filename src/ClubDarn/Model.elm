module ClubDarn.Model exposing (..)

import ClubDarn.Route as Route exposing (Route)
import Json.Decode exposing (bool, int, string, nullable, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import RemoteData exposing (WebData, RemoteData)
import LruCache exposing (LruCache)


type alias Model =
    { query : String
    , searchType : Route.SearchType
    , items : WebData PaginatedItems
    , route : Route
    , responseCache : LruCache String PaginatedItems
    }


type PaginatedItems
    = PaginatedSongs (Paginated Song)
    | PaginatedArtists (Paginated Artist)
    | PaginatedSeries (Paginated Series)


type alias Song =
    { id : Int
    , title : String
    , artist : Artist
    , dateAdded : Maybe String
    , endDate : Maybe String
    , lyrics : Maybe String
    , series : Maybe String
    , hasVideo : Bool
    , firstKana : Maybe String
    }


songDecoder : Decoder Song
songDecoder =
    decode Song
        |> required "id" int
        |> required "title" string
        |> required "artist" artistDecoder
        |> optional "dateAdded" (nullable string) Nothing
        |> optional "endDate" (nullable string) Nothing
        |> optional "lyrics" (nullable string) Nothing
        |> optional "series" (nullable string) Nothing
        |> optional "hasVideo" bool False
        |> optional "firstKana" (nullable string) Nothing


type alias Artist =
    { id : Int, name : String, firstKana : Maybe String }


artistDecoder : Decoder Artist
artistDecoder =
    decode Artist
        |> required "id" int
        |> required "name" string
        |> optional "firstKana" (nullable string) Nothing


type alias Series =
    { title : String, firstKana : Maybe String }


seriesDecoder : Decoder Series
seriesDecoder =
    decode Series
        |> required "title" string
        |> optional "firstKana" (nullable string) Nothing


type alias Paginated item =
    { page : Int
    , artistCategoryId : String
    , seriesCategoryId : Maybe String
    , totalItems : Int
    , totalPages : Int
    , items : List item
    }


paginatedDecoder : Decoder t -> Decoder (Paginated t)
paginatedDecoder itemDecoder =
    decode Paginated
        |> required "page" int
        |> required "artistCategoryId" string
        |> optional "seriesCategoryId" (nullable string) Nothing
        |> required "totalItems" int
        |> required "totalPages" int
        |> required "items" (list itemDecoder)
