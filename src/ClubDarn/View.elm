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
        , a [ Route.CategoryListing |> Route.reverse |> href ] [ text "Categories" ]
        , br [] []
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
            renderItems model

        Route.SongInfo songId ->
            renderItems model

        Route.CategoryListing ->
            renderItems model

        Route.ArtistSongs artistId ->
            renderItems model

        Route.CategorySongs categoryId ->
            renderItems model

        _ ->
            text "Other"


renderItems : Model -> Html Msg
renderItems model =
    case model.items of
        RemoteData.NotAsked ->
            text "..."

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Failure e ->
            div []
                [ text ("Error: " ++ toString e)
                , br [] []
                , button [ onClick Msgs.RetryRequest ] [ text "Retry" ]
                ]

        RemoteData.Success (Model.PaginatedSongs page) ->
            case model.route of
                Route.SongInfo songId ->
                    List.head page.items
                        |> Maybe.map renderSongInfo
                        |> Maybe.withDefault (div [] [ text "No song found" ])

                _ ->
                    page.items |> List.map renderSong |> ul []

        RemoteData.Success (Model.PaginatedArtists page) ->
            page.items |> List.map renderArtist |> ul []

        RemoteData.Success (Model.PaginatedSeries page) ->
            page.items |> List.map renderSeries |> ul []

        RemoteData.Success (Model.PaginatedCategoryGroups page) ->
            page.items |> List.map renderCategoryGroup |> ul []


renderSong : Model.Song -> Html Msg
renderSong song =
    li []
        [ a [ Route.SongInfo song.id |> Route.reverse |> href ]
            [ text song.title
            , text " - "
            , text song.artist.name
            ]
        ]


renderSongInfo : Model.Song -> Html Msg
renderSongInfo song =
    div []
        [ text (toString song)
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


renderCategoryGroup : Model.CategoryGroup -> Html Msg
renderCategoryGroup categoryGroup =
    li []
        [ text categoryGroup.description.en
        , ul [] (categoryGroup.categories |> List.map renderCategory)
        ]


renderCategory : Model.Category -> Html Msg
renderCategory category =
    li []
        [ a
            [ Route.CategorySongs category.id |> Route.reverse |> href ]
            [ text category.description.en ]
        ]
