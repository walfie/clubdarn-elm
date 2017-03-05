module ClubDarn.View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import ClubDarn.Msg as Msgs exposing (Msg)
import ClubDarn.Model as Model exposing (Model)
import ClubDarn.Route as Route exposing (Route)
import ClubDarn.Util as Util
import RemoteData exposing (RemoteData, WebData)
import Http
import Dict exposing (Dict)


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

        Route.SeriesSongs title ->
            renderItems model

        _ ->
            text "other"


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
            renderSongPage model.route page

        RemoteData.Success (Model.PaginatedArtists page) ->
            page.items |> List.map renderArtist |> ul []

        RemoteData.Success (Model.PaginatedSeries page) ->
            page.items |> List.map renderSeries |> ul []

        RemoteData.Success (Model.PaginatedCategoryGroups page) ->
            page.items |> List.map renderCategoryGroup |> ul []


renderSongPage : Route -> Model.Paginated Model.Song -> Html Msg
renderSongPage route page =
    case route of
        Route.SongInfo songId ->
            page.items
                |> List.head
                |> Maybe.map renderSongInfo
                |> Maybe.withDefault (div [] [ text "No song found" ])

        Route.ArtistSongs artistId ->
            div []
                [ page.items
                    |> List.head
                    |> Maybe.map (\s -> s.artist.name)
                    |> Util.orEmptyText
                , page.items |> List.map renderArtistSong |> ul []
                ]

        Route.SeriesSongs seriesTitle ->
            div []
                [ Http.decodeUri seriesTitle |> Util.orEmptyText
                , page.items |> List.map renderSong |> ul []
                ]

        Route.CategorySongs categoryId ->
            if categoryId >= "030000" && categoryId < "040000" then
                renderRecentSongs page
            else
                page.items
                    |> List.map renderRecentSong
                    |> ul []

        _ ->
            page.items |> List.map renderSong |> ul []


renderArtistSong : Model.Song -> Html Msg
renderArtistSong song =
    li []
        [ a [ Route.SongInfo song.id |> Route.reverse |> href ]
            [ text song.title ]
        ]


renderRecentSongs : Model.Paginated Model.Song -> Html Msg
renderRecentSongs page =
    let
        sortField =
            \s -> s.dateAdded |> Util.orEmptyString

        groupedByDate =
            page.items
                |> Util.groupBy sortField
                |> Dict.toList
                |> List.reverse
    in
        -- Wow this is a mess
        ul []
            (groupedByDate
                |> List.map
                    (\( date, songs ) ->
                        li []
                            [ div []
                                [ text date
                                , ul [] (List.map renderRecentSong songs)
                                ]
                            ]
                    )
            )


renderRecentSong : Model.Song -> Html Msg
renderRecentSong song =
    li []
        [ a [ Route.SongInfo song.id |> Route.reverse |> href ]
            [ text song.title
            , text " - "
            , song.series |> Maybe.withDefault song.artist.name |> text
            ]
        ]


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
        [ a
            [ Route.SeriesSongs series.title |> Route.reverse |> href ]
            [ text series.title ]
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
