module ClubDarn.Update exposing (..)

import ClubDarn.Msg exposing (Msg(..))
import ClubDarn.Model as Model exposing (Model)
import ClubDarn.Route as Route exposing (Route)
import Navigation
import Task
import Json.Decode as Decode
import Http
import RemoteData
import Json.Decode exposing (Decoder)
import LruCache exposing (LruCache)


-- TODO: Put in some config


apiBaseUrl =
    "http://localhost:8000/api"


seriesCategoryId =
    "050100"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange location ->
            let
                route =
                    Route.parseLocation location
            in
                handleLocationChange { model | route = route, query = "" }

        RetryRequest ->
            handleLocationChange model

        QueryInput query ->
            { model | query = query } ! []

        ChangeSearchType searchType ->
            { model | searchType = searchType } ! []

        QuerySubmit ->
            let
                query =
                    if (String.isEmpty model.query) then
                        Nothing
                    else
                        (Just model.query)

                newRoute =
                    Route.SearchResults model.searchType query
            in
                model ! [ newRoute |> Route.reverse |> Navigation.newUrl ]

        ApiResult url result ->
            let
                newModel =
                    { model | items = RemoteData.fromResult result }
            in
                case result of
                    Ok page ->
                        let
                            updatedCache =
                                model.responseCache |> LruCache.insert url page
                        in
                            { newModel | responseCache = updatedCache } ! []

                    _ ->
                        newModel ! []


handleLocationChange : Model -> ( Model, Cmd Msg )
handleLocationChange model =
    case model.route of
        Route.CategoryListing ->
            handleSearch model
                "/categories"
                Model.categoryGroupDecoder
                Model.PaginatedCategoryGroups

        Route.SearchResults (Route.SongSearch) (Just query) ->
            handleSearch model
                ("/songs/?title=" ++ query)
                Model.songDecoder
                Model.PaginatedSongs

        Route.SearchResults (Route.ArtistSearch) (Just query) ->
            handleSearch model
                ("/artists/?name=" ++ query)
                Model.artistDecoder
                Model.PaginatedArtists

        Route.SearchResults (Route.SeriesSearch) (Just query) ->
            handleSearch model
                ("/series/?title=" ++ query)
                Model.seriesDecoder
                Model.PaginatedSeries

        Route.SongInfo songId ->
            case model.items of
                RemoteData.Success (Model.PaginatedSongs page) ->
                    let
                        newItems =
                            page.items |> List.filter (\s -> s.id == songId)

                        newResult =
                            Model.PaginatedSongs { page | items = newItems }
                    in
                        { model | items = RemoteData.Success newResult } ! []

                _ ->
                    handleSearch model
                        ("/songs/" ++ toString songId ++ "?")
                        Model.songDecoder
                        Model.PaginatedSongs

        Route.ArtistSongs artistId ->
            handleSearch model
                ("/artists/" ++ toString artistId ++ "/songs?")
                Model.songDecoder
                Model.PaginatedSongs

        Route.CategorySongs categoryId ->
            handleSearch model
                ("/categories/" ++ categoryId ++ "/songs?")
                Model.songDecoder
                Model.PaginatedSongs

        Route.SeriesSongs seriesTitle ->
            handleSearch model
                ("/categories/" ++ seriesCategoryId ++ "/series/" ++ seriesTitle ++ "/songs?")
                Model.songDecoder
                Model.PaginatedSongs

        _ ->
            model ! []


handleSearch model path itemDecoder itemType =
    let
        -- TODO: serial_no
        url =
            apiBaseUrl ++ path

        ( updatedCache, cachedPage ) =
            model.responseCache |> LruCache.get url
    in
        case cachedPage of
            Just page ->
                ( { model
                    | items = RemoteData.Success page
                    , responseCache = updatedCache
                  }
                , Cmd.none
                )

            Nothing ->
                let
                    pageDecoder =
                        Model.paginatedDecoder itemDecoder |> Decode.map itemType

                    request =
                        Http.get url pageDecoder
                in
                    ( { model | items = RemoteData.Loading }
                    , Http.send (ApiResult url) request
                    )
