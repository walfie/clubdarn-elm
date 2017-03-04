module ClubDarn.View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import ClubDarn.Msg as Msgs exposing (Msg)
import ClubDarn.Model as Model exposing (Model)
import ClubDarn.Route as Route exposing (Route)
import RemoteData exposing (RemoteData, WebData)


view : Model -> Html Msg
view model =
    div []
        [ searchInput model
        , searchSelect model
        , mainContent model
        ]


searchSelect : Model -> Html Msg
searchSelect model =
    Html.fieldset []
        [ radio model "Song" Route.SongSearch
        , radio model "Artist" Route.ArtistSearch
        , radio model "Series" Route.SeriesSearch
        ]


radio : Model -> String -> Route.SearchType -> Html Msg
radio model value searchType =
    let
        isChecked =
            model.searchType == searchType
    in
        label []
            [ input
                [ type_ "radio"
                , onClick (Msgs.ChangeSearchType searchType)
                , checked isChecked
                ]
                []
            , text value
            ]


searchInput : Model -> Html Msg
searchInput model =
    Html.form [ onSubmit Msgs.QuerySubmit ]
        [ input [ onInput Msgs.QueryInput, value model.query ] []
        , button [] [ text "Search" ]
        ]


mainContent : Model -> Html Msg
mainContent model =
    case model.route of
        Route.SearchResults _ query ->
            renderItems model.items

        Route.CategoryListing ->
            text "Categories"

        Route.ArtistSongs artistId ->
            renderItems model.items

        _ ->
            text "Other"


renderItems : WebData Model.PaginatedItems -> Html Msg
renderItems webData =
    case webData of
        RemoteData.NotAsked ->
            text "..."

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Failure e ->
            text ("Error: " ++ toString e)

        RemoteData.Success (Model.PaginatedSongs page) ->
            page.items |> List.map renderSong |> ul []

        RemoteData.Success (Model.PaginatedArtists page) ->
            page.items |> List.map renderArtist |> ul []

        RemoteData.Success (Model.PaginatedSeries page) ->
            page.items |> List.map renderSeries |> ul []


renderSong : Model.Song -> Html Msg
renderSong song =
    li []
        [ text song.title
        , text song.artist.name
        ]


renderArtist : Model.Artist -> Html Msg
renderArtist artist =
    li []
        [ a [ Route.ArtistSongs artist.id |> Route.reverse |> href ] [ text artist.name ]
        ]


renderSeries : Model.Series -> Html Msg
renderSeries series =
    li []
        [ text series.title
        ]
