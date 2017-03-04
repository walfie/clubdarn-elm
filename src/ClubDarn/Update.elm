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


-- TODO: Put in some config


apiBaseUrl : String
apiBaseUrl =
    "http://localhost:8000/api"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange location ->
            let
                route =
                    Route.parseLocation location
            in
                handleLocationChange { model | route = route, query = "" }

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

        ApiResult result ->
            { model | items = RemoteData.fromResult result } ! []



-- TODO: serial_no


handleLocationChange : Model -> ( Model, Cmd Msg )
handleLocationChange model =
    case model.route of
        Route.SearchResults (Route.SongSearch) (Just query) ->
            handleSearch model ("/songs/?title=" ++ query) Model.songDecoder Model.PaginatedSongs

        Route.SearchResults (Route.ArtistSearch) (Just query) ->
            handleSearch model ("/artists/?name=" ++ query) Model.artistDecoder Model.PaginatedArtists

        Route.SearchResults (Route.SeriesSearch) (Just query) ->
            model ! []

        Route.ArtistSongs artistId ->
            model ! []

        Route.CategorySongs categoryId ->
            model ! []

        Route.SeriesSongs seriesTitle ->
            model ! []

        _ ->
            model ! []


handleSearch model path itemDecoder itemType =
    let
        url =
            apiBaseUrl ++ path

        pageDecoder =
            Model.paginatedDecoder itemDecoder |> Decode.map itemType

        request =
            Http.get url pageDecoder
    in
        { model | items = RemoteData.Loading } ! [ Http.send ApiResult request ]
