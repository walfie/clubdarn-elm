module ClubDarn.Route exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing ((<?>), (</>), int, s, string, stringParam)
import Http
import ClubDarn.Util as Util
import Dict exposing (Dict)


type alias Query =
    String


type alias ArtistId =
    Int


type alias SongId =
    Int


type alias CategoryId =
    String


type alias SeriesTitle =
    String


type SearchType
    = SongSearch
    | ArtistSearch
    | SeriesSearch


type Route
    = MainSearch
    | SearchResults SearchType (Maybe Query)
    | SongInfo SongId
    | CategoryListing
    | SeriesListing CategoryId
    | ArtistSongs ArtistId
    | CategorySongs CategoryId
    | SeriesSongs CategoryId SeriesTitle
    | SimilarSongs SongId
    | FileSearch
    | Settings
    | NotFound


reverse : Route -> String
reverse route =
    "#/"
        ++ case route of
            MainSearch ->
                ""

            SongInfo songId ->
                "songs/" ++ toString songId

            SimilarSongs songId ->
                "songs/" ++ toString songId ++ "/similar"

            SearchResults SongSearch query ->
                Util.maybeFold ((++) "?title=") "" query
                    |> String.append "songs"

            SearchResults ArtistSearch query ->
                Util.maybeFold ((++) "?name=") "" query
                    |> String.append "artists"

            SearchResults SeriesSearch query ->
                Util.maybeFold ((++) "?title=") "" query
                    |> String.append "series"

            ArtistSongs artistId ->
                "artists/" ++ (toString artistId) ++ "/songs"

            CategorySongs categoryId ->
                "categories/" ++ categoryId ++ "/songs"

            SeriesSongs categoryId seriesTitle ->
                "categories/" ++ categoryId ++ "/series/" ++ (Http.encodeUri seriesTitle) ++ "/songs"

            CategoryListing ->
                "categories"

            SeriesListing categoryId ->
                "categories/" ++ categoryId ++ "/series"

            Settings ->
                "settings"

            FileSearch ->
                "files"

            NotFound ->
                ""


type alias Tabs =
    { mainSearch : Int
    , categoryListing : Int
    , fileSearch : Int
    , settings : Int
    }


tabs : Tabs
tabs =
    { mainSearch = 0
    , categoryListing = 1
    , fileSearch = 2
    , settings = 3
    }


activeTab : Route -> Int
activeTab route =
    case route of
        MainSearch ->
            tabs.mainSearch

        CategoryListing ->
            tabs.categoryListing

        FileSearch ->
            tabs.fileSearch

        Settings ->
            tabs.settings

        _ ->
            -1


tabDict : Dict Int Route
tabDict =
    Dict.fromList
        [ ( tabs.mainSearch, MainSearch )
        , ( tabs.categoryListing, CategoryListing )
        , ( tabs.fileSearch, FileSearch )
        , ( tabs.settings, Settings )
        ]


matchers : Url.Parser (Route -> a) a
matchers =
    Url.oneOf
        [ Url.map MainSearch Url.top
        , Url.map MainSearch (s "")
        , Url.map (SearchResults SongSearch) (s "songs" <?> stringParam "title")
        , Url.map (SearchResults ArtistSearch) (s "artists" <?> stringParam "name")
        , Url.map (SearchResults SeriesSearch) (s "series" <?> stringParam "title")
        , Url.map SongInfo (s "songs" </> int)
        , Url.map SimilarSongs (s "songs" </> int </> s "similar")
        , Url.map ArtistSongs (s "artists" </> int </> s "songs")
        , Url.map CategorySongs (s "categories" </> string </> s "songs")
        , Url.map SeriesSongs (s "categories" </> string </> s "series" </> string </> s "songs")
        , Url.map CategoryListing (s "categories")
        , Url.map SeriesListing (s "categories" </> string </> s "series")
        , Url.map FileSearch (s "files")
        , Url.map Settings (s "settings")
        ]


parseLocation : Location -> Route
parseLocation location =
    hashQuery location
        |> Url.parseHash matchers
        |> Maybe.withDefault NotFound


{-| UrlParser doesn't handle query strings inside the hash, so we fake it
-}
hashQuery : Location -> Location
hashQuery location =
    let
        ( path, query ) =
            case (String.split "?" location.hash) of
                path :: queryParts ->
                    ( path, "?" ++ (String.join "?" queryParts) )

                [] ->
                    ( "", "" )
    in
        { location | hash = path, search = query }
