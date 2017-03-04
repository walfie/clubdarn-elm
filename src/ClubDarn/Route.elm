module ClubDarn.Route exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing ((<?>), (</>), int, s, string, stringParam)
import Http


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
    = SearchResults SearchType (Maybe Query)
    | ArtistSongs ArtistId
    | CategorySongs CategoryId
    | SeriesSongs SeriesTitle
    | CategoryListing
    | SongInfo SongId
    | NotFound


maybeFold : (a -> b) -> b -> Maybe a -> b
maybeFold f default opt =
    opt
        |> Maybe.map f
        |> Maybe.withDefault default


reverse : Route -> String
reverse route =
    "#/"
        ++ case route of
            SongInfo songId ->
                "songs/" ++ toString songId

            SearchResults SongSearch query ->
                maybeFold ((++) "?title=") "" query
                    |> String.append "songs"

            SearchResults ArtistSearch query ->
                maybeFold ((++) "?name=") "" query
                    |> String.append "artists"

            SearchResults SeriesSearch query ->
                maybeFold ((++) "?title=") "" query
                    |> String.append "series"

            ArtistSongs artistId ->
                "artists/" ++ (toString artistId) ++ "/songs"

            CategorySongs categoryId ->
                "categories/" ++ categoryId ++ "/songs"

            SeriesSongs seriesTitle ->
                "series/" ++ (Http.encodeUri seriesTitle) ++ "/songs"

            CategoryListing ->
                "categories"

            NotFound ->
                ""


matchers : Url.Parser (Route -> a) a
matchers =
    Url.oneOf
        [ Url.map (SearchResults SongSearch Nothing) Url.top
        , Url.map (SearchResults SongSearch) (s "songs" <?> stringParam "title")
        , Url.map (SearchResults ArtistSearch) (s "artists" <?> stringParam "name")
        , Url.map (SearchResults SeriesSearch) (s "series" <?> stringParam "title")
        , Url.map SongInfo (s "songs" </> int)
        , Url.map ArtistSongs (s "artists" </> int </> s "songs")
        , Url.map CategorySongs (s "categories" </> string </> s "songs")
        , Url.map SeriesSongs (s "series" </> string </> s "songs")
        , Url.map CategoryListing (s "categories")
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
