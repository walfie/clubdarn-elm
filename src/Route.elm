module Route exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing ((<?>), s, stringParam)


type alias Query =
    String


type Route
    = SongSearch (Maybe Query)
    | ArtistSearch (Maybe Query)
    | SeriesSearch (Maybe Query)
    | CategoryListing
    | NotFound


matchers : Url.Parser (Route -> a) a
matchers =
    Url.oneOf
        [ Url.map (SongSearch Nothing) Url.top
        , Url.map SongSearch (s "songs" <?> stringParam "title")
        , Url.map ArtistSearch (s "artists" <?> stringParam "name")
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
