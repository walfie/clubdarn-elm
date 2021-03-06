module ClubDarn.Model exposing (..)

import ClubDarn.Route as Route exposing (Route)
import Json.Decode exposing (bool, int, string, nullable, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
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
    , searchTabTarget : Route
    , fileSearchState : Maybe FileSearchState
    , apiBaseUrl : String
    , settings : Settings
    , mdl : Material.Model
    }


type alias FileSearchState =
    { metadata : List TitleAndArtist
    , progress : Int
    , total : Int
    , items : WebData PaginatedItems
    }


getSeriesCategoryId : Model -> Maybe String
getSeriesCategoryId model =
    let
        getCategoryIdFromPage : PaginatedItems -> Maybe String
        getCategoryIdFromPage page =
            case page of
                PaginatedSongs items ->
                    items.seriesCategoryId

                PaginatedSeries items ->
                    items.seriesCategoryId

                _ ->
                    Nothing
    in
        case model.items of
            RemoteData.Success page ->
                getCategoryIdFromPage page

            _ ->
                Nothing


type alias TitleAndArtist =
    { title : String, artist : String }


titleAndArtistEncoder : TitleAndArtist -> Encode.Value
titleAndArtistEncoder data =
    Encode.object
        [ ( "title", Encode.string data.title )
        , ( "artist", Encode.string data.artist )
        ]


type alias Flags =
    { apiBaseUrl : String
    , settings : Maybe Settings
    }


type alias Settings =
    { serialNo : Maybe String }


type alias KaraokeMachine =
    { name : String, serialNo : Maybe String }


karaokeMachines : List KaraokeMachine
karaokeMachines =
    [ { name = "LiveDAM", serialNo = Nothing }
    , { name = "PremierDAM", serialNo = Just "AB316238" }
    ]


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


type CategoryType
    = SongCategory
    | SeriesCategory


type alias CategoryGroup =
    { description : Description
    , categories : List Category
    , categoryType : CategoryType
    }


categoryGroupDecoder : Decoder CategoryGroup
categoryGroupDecoder =
    decode CategoryGroup
        |> required "description" descriptionDecoder
        |> required "categories" (list categoryDecoder)
        |> hardcoded SongCategory


{-| Hardcoding this because changing the server to return it is too much effort
-}
musicVideoSeriesCategoryGroup : CategoryGroup
musicVideoSeriesCategoryGroup =
    { description = { ja = "アニメ･特撮", en = "Series" }
    , categories =
        [ { id = "050300"
          , description = { ja = "映像", en = "Music Video" }
          }
        ]
    , categoryType = SeriesCategory
    }


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
