module ClubDarn.View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import ClubDarn.Msg as Msg exposing (Msg)
import ClubDarn.Model as Model exposing (Model)
import ClubDarn.Route as Route exposing (Route)
import ClubDarn.Util as Util
import RemoteData exposing (RemoteData, WebData)
import Http
import Dict exposing (Dict)
import Material.Layout as Layout
import Material.Toggles as Toggles
import Material.Options as Options
import Material.Button as Button
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Color as Color


view : Model -> Html Msg
view model =
    Layout.render Msg.Mdl
        model.mdl
        [ Layout.fixedHeader ]
        { header = header model
        , drawer =
            [ a
                [ Route.CategoryListing |> Route.reverse |> href ]
                [ text "Categories" ]
            ]
        , tabs = ( [], [] )
        , main =
            [ Grid.grid []
                [ Grid.cell [ Grid.size Grid.All 1, Options.css "margin" "0" ] []
                , Grid.cell [ Grid.size Grid.All 10 ] [ mainContent model ]
                ]
            ]
        }


header : Model -> List (Html Msg)
header model =
    [ Layout.row [ Options.css "height" "inherit" ]
        [ Layout.spacer
        , Grid.grid []
            [ Grid.cell [ Grid.size Grid.All 6 ] [ searchInput model ]
            , Grid.cell [ Grid.size Grid.All 6 ] (searchSelect model)
            ]
        ]
    ]


searchSelect : Model -> List (Html Msg)
searchSelect model =
    let
        options =
            [ ( 1, "Song", Route.SongSearch )
            , ( 2, "Artist", Route.ArtistSearch )
            , ( 3, "Series", Route.SeriesSearch )
            ]

        toButton =
            \( id, name, searchType ) ->
                let
                    defaultOptions =
                        [ Button.ripple
                        , Options.onClick (Msg.ChangeSearchType searchType)
                        ]

                    extraOptions =
                        if (searchType == model.searchType) then
                            [ Options.cs "mdl-color--white", Button.raised ]
                        else
                            [ Options.cs "mdl-color-text--white" ]
                in
                    Button.render Msg.Mdl
                        [ id ]
                        model.mdl
                        (defaultOptions ++ extraOptions)
                        [ text name ]
    in
        options |> List.map toButton


searchInput : Model -> Html Msg
searchInput model =
    Html.form [ onSubmit Msg.QuerySubmit ]
        [ input [ onInput Msg.QueryInput, value model.query ] []
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
                , button [ onClick Msg.RetryRequest ] [ text "Retry" ]
                ]

        RemoteData.Success (Model.PaginatedSongs page) ->
            renderSongPage model.route page

        RemoteData.Success (Model.PaginatedArtists page) ->
            page.items |> List.map renderArtist |> ul []

        RemoteData.Success (Model.PaginatedSeries page) ->
            page.items |> List.map renderSeries |> mainGrid

        RemoteData.Success (Model.PaginatedCategoryGroups page) ->
            page.items |> List.map renderCategoryGroup |> ul []


mainGrid : List (Html Msg) -> Html Msg
mainGrid items =
    items
        |> List.map List.singleton
        |> List.map (Grid.cell [ Grid.size Grid.All 4 ])
        |> Grid.grid []


renderItem : Route -> List (Html Msg) -> Html Msg
renderItem onClickRoute innerContents =
    a
        [ onClickRoute |> Route.reverse |> href
        , class "darn-search-item--link"
        ]
        [ Options.div
            [ Elevation.e4
            , Options.cs "darn-search-item--container"
            ]
            innerContents
        ]


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
                [ Http.decodeUri seriesTitle |> Util.orEmptyString |> itemsHeader
                , page.items |> List.map renderSong |> mainGrid
                ]

        Route.CategorySongs categoryId ->
            if categoryId >= "030000" && categoryId < "040000" then
                renderRecentSongs page
            else
                page.items
                    |> List.map renderRecentSong
                    |> ul []

        _ ->
            page.items |> List.map renderSong |> mainGrid


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


itemTitle : String -> Html Msg
itemTitle string =
    Options.div
        [ Options.cs "darn-search-item__title" ]
        [ text string ]


itemsHeader : String -> Html Msg
itemsHeader string =
    Options.div
        [ Options.cs "darn-search-header", Color.text Color.primaryDark ]
        [ text string ]


itemIcon : String -> Html Msg
itemIcon icon =
    i [ class "material-icons", class "darn-search-item__icon" ] [ text icon ]


renderSong : Model.Song -> Html Msg
renderSong song =
    renderItem (Route.SongInfo song.id)
        [ itemTitle song.title
        , div []
            [ itemIcon "person"
            , span [] [ text song.artist.name ]
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
    renderItem (Route.SeriesSongs series.title) [ itemTitle series.title ]


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
