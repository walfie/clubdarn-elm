module ClubDarn.Update exposing (..)

import ClubDarn.Msg exposing (Msg(..))
import ClubDarn.Model as Model exposing (Model)
import ClubDarn.Route as Route exposing (Route)
import ClubDarn.Util as Util
import Navigation
import Task
import Json.Decode as Decode
import Http
import RemoteData
import Json.Decode exposing (Decoder)
import LruCache exposing (LruCache)
import Material
import QueryString exposing (QueryString)


-- TODO: Put in some config


apiBaseUrl =
    "http://localhost:8000/api"


seriesCategoryId =
    "050100"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl m ->
            Material.update Mdl m model

        UpdateSettings settings ->
            -- TODO: Update localstorage
            { model | settings = settings } ! []

        SelectTab tabNumber ->
            case tabNumber of
                0 ->
                    model ! [ Route.reverse Route.MainSearch |> Navigation.newUrl ]

                1 ->
                    model ! [ Route.reverse Route.CategoryListing |> Navigation.newUrl ]

                2 ->
                    model ! [ Route.reverse Route.Settings |> Navigation.newUrl ]

                _ ->
                    model ! []

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

        ShowSong song ->
            { model | activeSong = song } ! []


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
                QueryString.empty
                Model.categoryGroupDecoder
                Model.PaginatedCategoryGroups

        Route.SearchResults (Route.SongSearch) (Just query) ->
            handleSearch model
                "/songs/"
                (apiQuery model [ ( "title", query ) ])
                Model.songDecoder
                Model.PaginatedSongs

        Route.SearchResults (Route.ArtistSearch) (Just query) ->
            handleSearch model
                "/artists/"
                (apiQuery model [ ( "name", query ) ])
                Model.artistDecoder
                Model.PaginatedArtists

        Route.SearchResults (Route.SeriesSearch) (Just query) ->
            handleSearch model
                "/series/"
                (apiQuery model [ ( "title", query ) ])
                Model.seriesDecoder
                Model.PaginatedSeries

        Route.SongInfo songId ->
            handleSearch model
                ("/songs/" ++ toString songId)
                QueryString.empty
                Model.songDecoder
                Model.PaginatedSongs

        Route.ArtistSongs artistId ->
            handleSearch model
                ("/artists/" ++ toString artistId ++ "/songs")
                QueryString.empty
                Model.songDecoder
                Model.PaginatedSongs

        Route.CategorySongs categoryId ->
            handleSearch model
                ("/categories/" ++ categoryId ++ "/songs")
                QueryString.empty
                Model.songDecoder
                Model.PaginatedSongs

        Route.SeriesSongs seriesTitle ->
            handleSearch model
                ("/categories/" ++ seriesCategoryId ++ "/series/" ++ seriesTitle ++ "/songs")
                QueryString.empty
                Model.songDecoder
                Model.PaginatedSongs

        Route.SimilarSongs songId ->
            handleSearch model
                ("/songs/" ++ toString songId ++ "/similar")
                QueryString.empty
                Model.songDecoder
                Model.PaginatedSongs

        _ ->
            model ! []


handleSearch :
    Model
    -> String
    -> QueryString
    -> Decoder t
    -> (Model.Paginated t -> Model.PaginatedItems)
    -> ( Model, Cmd Msg )
handleSearch model path query itemDecoder itemType =
    let
        -- TODO: serial_no
        url =
            apiBaseUrl ++ path ++ (QueryString.render query)

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
