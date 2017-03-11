module ClubDarn.Model exposing (..)

import ClubDarn.Route as Route exposing (Route)
import Json.Decode exposing (bool, int, string, nullable, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import RemoteData exposing (WebData, RemoteData)
import LruCache exposing (LruCache)
import Material


type alias Model =
    { query : String
    , searchType : Route.SearchType
    , items : WebData PaginatedItems
    , route : Route
    , activeSong : Maybe Song
    , responseCache : LruCache String PaginatedItems
    , mdl : Material.Model
    }


type PaginatedItems
    = PaginatedSongs (Paginated Song)
    | PaginatedArtists (Paginated Artist)
    | PaginatedSeries (Paginated Series)
    | PaginatedCategoryGroups (Paginated CategoryGroup)


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
    -- TODO: Handle case where `dateAdded` is "1970/01/01"
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


type alias CategoryGroup =
    { description : Description
    , categories : List Category
    }


categoryGroupDecoder : Decoder CategoryGroup
categoryGroupDecoder =
    decode CategoryGroup
        |> required "description" descriptionDecoder
        |> required "categories" (list categoryDecoder)


type alias Description =
    { ja : String, en : String }


descriptionDecoder : Decoder Description
descriptionDecoder =
    decode Description
        |> required "ja" string
        |> required "en" string


type alias Category =
    { id : String, description : Description }


categoryDecoder : Decoder Category
categoryDecoder =
    decode Category
        |> required "id" string
        |> required "description" descriptionDecoder


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
