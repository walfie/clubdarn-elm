port module ClubDarn.Update exposing (..)

import ClubDarn.Model as Model exposing (Model)
import ClubDarn.Msg as Msg exposing (Msg(..))
import ClubDarn.Route as Route exposing (Route, tabs, tabDict)
import ClubDarn.Util as Util
import Dict
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LruCache exposing (LruCache)
import Material
import Navigation
import QueryString exposing (QueryString)
import RemoteData
import Task


apiBaseUrl =
    "http://localhost:8000/api"



-- TODO: Put in some config


defaultSeriesCategoryId =
    "050100"


port saveSettings : Model.Settings -> Cmd msg


port selectFile : Msg.ElementId -> Cmd msg


port fileMetadata : (Msg.FileMetadata -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl m ->
            Material.update Mdl m model

        UpdateSettings settings ->
            { model | settings = settings } ! [ saveSettings settings ]

        SelectTab tabNumber ->
            let
                -- TODO: Add route for "not found"
                newRoute =
                    Dict.get tabNumber tabDict |> Maybe.withDefault Route.MainSearch
            in
                model ! [ Route.reverse newRoute |> Navigation.newUrl ]

        LocationChange location ->
            let
                route =
                    Route.parseLocation location
            in
                handleLocationChange { model | route = route, activeSong = Nothing }

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
                newItems =
                    -- Manually add the music videos category group to the response
                    case RemoteData.fromResult result of
                        RemoteData.Success (Model.PaginatedCategoryGroups page) ->
                            { page | items = page.items ++ [ Model.musicVideoSeriesCategoryGroup ] }
                                |> Model.PaginatedCategoryGroups
                                |> RemoteData.Success

                        other ->
                            other

                newModel =
                    if model.route == Route.FileSearch then
                        let
                            updateState : Model.FileSearchState -> Model.FileSearchState
                            updateState state =
                                { state | items = newItems }
                        in
                            { model | fileSearchState = model.fileSearchState |> Maybe.map updateState }
                    else
                        { model | items = newItems }
            in
                case newItems of
                    RemoteData.Success page ->
                        let
                            updatedCache =
                                model.responseCache |> LruCache.insert url page
                        in
                            { newModel | responseCache = updatedCache } ! []

                    _ ->
                        newModel ! []

        ShowSong song ->
            { model | activeSong = song } ! []

        SelectFile inputId ->
            { model | fileSearchState = Nothing } ! [ selectFile inputId ]

        ReceiveFileMetadata metadata ->
            handleFileMetadata model metadata


apiQuery : Model -> List ( String, String ) -> QueryString
apiQuery model params =
    let
        base =
            case model.settings.serialNo of
                Just s ->
                    QueryString.empty |> QueryString.add "serial_no" s

                Nothing ->
                    QueryString.empty
    in
        List.foldl (uncurry QueryString.add) base params


handleLocationChange : Model -> ( Model, Cmd Msg )
handleLocationChange model =
    case model.route of
        Route.CategoryListing ->
            handleSearch model
                "/categories"
                []
                Model.categoryGroupDecoder
                Model.PaginatedCategoryGroups

        Route.SeriesListing categoryId ->
            handleSearch model
                ("/categories/" ++ categoryId ++ "/series")
                []
                Model.seriesDecoder
                Model.PaginatedSeries

        Route.SearchResults (Route.SongSearch) (Just query) ->
            handleSearch model
                "/songs/"
                [ ( "title", query ) ]
                Model.songDecoder
                Model.PaginatedSongs

        Route.SearchResults (Route.ArtistSearch) (Just query) ->
            handleSearch model
                "/artists/"
                [ ( "name", query ) ]
                Model.artistDecoder
                Model.PaginatedArtists

        Route.SearchResults (Route.SeriesSearch) (Just query) ->
            handleSearch model
                "/series/"
                [ ( "title", query ) ]
                Model.seriesDecoder
                Model.PaginatedSeries

        Route.SongInfo songId ->
            handleSearch model
                ("/songs/" ++ toString songId)
                []
                Model.songDecoder
                Model.PaginatedSongs

        Route.ArtistSongs artistId ->
            handleSearch model
                ("/artists/" ++ toString artistId ++ "/songs")
                []
                Model.songDecoder
                Model.PaginatedSongs

        Route.CategorySongs categoryId ->
            handleSearch model
                ("/categories/" ++ categoryId ++ "/songs")
                []
                Model.songDecoder
                Model.PaginatedSongs

        Route.SeriesSongs categoryId seriesTitle ->
            handleSearch model
                ("/categories/" ++ categoryId ++ "/series/" ++ seriesTitle ++ "/songs")
                []
                Model.songDecoder
                Model.PaginatedSongs

        Route.SimilarSongs songId ->
            handleSearch model
                ("/songs/" ++ toString songId ++ "/similar")
                []
                Model.songDecoder
                Model.PaginatedSongs

        _ ->
            model ! []


handleSearch :
    Model
    -> String
    -> List ( String, String )
    -> Decoder t
    -> (Model.Paginated t -> Model.PaginatedItems)
    -> ( Model, Cmd Msg )
handleSearch model path queryParams itemDecoder itemType =
    let
        query =
            apiQuery model queryParams

        url =
            apiBaseUrl ++ path ++ (QueryString.render query)

        ( updatedCache, cachedPage ) =
            LruCache.get url model.responseCache
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


handleFileMetadata : Model -> Msg.FileMetadata -> ( Model, Cmd Msg )
handleFileMetadata model metadata =
    let
        newItem =
            Util.maybeToList metadata.result

        newState =
            case model.fileSearchState of
                Nothing ->
                    { metadata = newItem
                    , progress = 1
                    , total = metadata.total
                    , items = RemoteData.NotAsked
                    }

                Just state ->
                    { state
                        | metadata = newItem ++ state.metadata
                        , progress = state.progress + 1
                        , total = metadata.total
                    }

        newModel =
            { model | fileSearchState = Just newState }
    in
        if newState.progress == newState.total then
            let
                query =
                    apiQuery model []

                url =
                    apiBaseUrl ++ "/songs/lookup" ++ (QueryString.render query)

                decoder =
                    Model.paginatedDecoder Model.songDecoder
                        |> Decode.map Model.PaginatedSongs

                body =
                    newState.metadata
                        |> List.map Model.titleAndArtistEncoder
                        |> Encode.list
                        |> Http.jsonBody

                cmd =
                    Http.post url body decoder
                        |> Http.send (ApiResult url)

                loadingState =
                    { newState | items = RemoteData.Loading }
            in
                { model | fileSearchState = Just loadingState } ! [ cmd ]
        else
            { model | fileSearchState = Just newState } ! []
