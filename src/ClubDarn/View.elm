module ClubDarn.View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import ClubDarn.Msg as Msg exposing (Msg)
import ClubDarn.Model as Model exposing (Model)
import ClubDarn.Route as Route exposing (Route)
import ClubDarn.Util as Util
import ClubDarn.Update exposing (defaultSeriesCategoryId)
import RemoteData exposing (RemoteData, WebData)
import Navigation
import Http
import Dict exposing (Dict)
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Layout as Layout
import Material.List as MdlList
import Material.Options as Options
import Material.Spinner as Spinner
import Material.Tabs as Tabs
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Typography as Typography


view : Model -> Html Msg
view model =
    Layout.render Msg.Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.fixedTabs
        , Layout.onSelectTab Msg.SelectTab
        , Layout.selectedTab (Route.activeTab model.route)
        ]
        { header = []
        , drawer = []
        , tabs =
            ( [ "search", "format_list_numbered", "settings" ]
                |> List.map (flip Icon.view [ Options.cs "darn-tab-icon" ])
            , []
            )
        , main =
            [ renderSongDialog model
            , renderSongDialogOverlay model
            , Grid.grid [ Options.cs "darn-main-content__container" ]
                [ Grid.cell
                    [ Grid.size Grid.All 10, Options.cs "darn-main-content" ]
                    (mainContent model)
                ]
            ]
        }


mainContent : Model -> List (Html Msg)
mainContent model =
    case model.route of
        Route.CategoryListing ->
            [ renderItems model ]

        Route.CategorySongs _ ->
            [ renderItems model ]

        Route.MainSearch ->
            [ renderSearchBox model ]

        Route.Settings ->
            [ centeredColumn [ itemsHeader "Settings", renderSettings model ] ]

        _ ->
            [ renderSearchBox model, renderItems model ]


renderSongDialogOverlay : Model -> Html Msg
renderSongDialogOverlay model =
    if model.activeSong == Nothing then
        text ""
    else
        div
            [ class "darn-dialog__overlay", onClick (Msg.ShowSong Nothing) ]
            []


renderSongDialog : Model -> Html Msg
renderSongDialog model =
    case model.activeSong of
        Nothing ->
            text ""

        Just song ->
            Options.div [ Options.cs "darn-dialog", Elevation.e8 ]
                [ a
                    [ class "darn-dialog__title"
                    , Route.reverse (Route.SongInfo song.id) |> href
                    ]
                    [ displaySongId song.id |> text ]
                , renderSongDialogContents (Model.getSeriesCategoryId model) song
                , div [ class "darn-dialog__actions" ]
                    [ Button.render Msg.Mdl
                        [ 2 ]
                        model.mdl
                        [ Options.onClick (Msg.ShowSong Nothing) ]
                        [ text "Close" ]
                    , a
                        [ Route.reverse (Route.SimilarSongs song.id) |> href ]
                        [ Button.render Msg.Mdl
                            [ 3 ]
                            model.mdl
                            []
                            [ text "Find Similar" ]
                        ]
                    ]
                ]


renderSongDialogContents : Maybe String -> Model.Song -> Html Msg
renderSongDialogContents seriesCategoryId song =
    let
        listItem : String -> Html Msg -> Html Msg
        listItem icon contents =
            MdlList.li [ Options.cs "darn-dialog__list-item" ]
                [ MdlList.content []
                    [ MdlList.icon icon [ Options.cs "darn-dialog__list-icon" ]
                    , contents
                    ]
                ]

        linkedItem : Route -> String -> Html Msg
        linkedItem route contents =
            a
                [ Route.reverse route |> href, class "darn-dialog__list-item--linked" ]
                [ text contents ]

        seriesItemFromTitle : String -> Html Msg
        seriesItemFromTitle title =
            linkedItem (Route.SeriesSongs defaultSeriesCategoryId title) title

        maybeListItems =
            [ listItem "audiotrack" (text song.title) |> Just
            , linkedItem (Route.ArtistSongs song.artist.id) song.artist.name
                |> listItem "person"
                |> Just
            , Maybe.map (listItem "local_movies" << seriesItemFromTitle) song.series
            , Maybe.map (listItem "date_range" << text) song.dateAdded
            , Maybe.map (listItem "textsms" << text) song.lyrics
            ]
    in
        MdlList.ul [] (Util.flattenListOfMaybe maybeListItems)


displaySongId : Int -> String
displaySongId songId =
    let
        string =
            toString songId
    in
        (String.slice 0 4 string) ++ "-" ++ (String.slice 4 6 string)


renderSearchBox : Model -> Html Msg
renderSearchBox model =
    div [ class "darn-search-box" ]
        [ searchBox model ]


searchSelect : Model -> List (Html Msg)
searchSelect model =
    let
        options =
            [ ( 1, "Song", Route.SongSearch )
            , ( 2, "Artist", Route.ArtistSearch )
            , ( 3, "Series", Route.SeriesSearch )
            ]

        toRadio : ( Int, String, Route.SearchType ) -> Html Msg
        toRadio ( id, name, searchType ) =
            Toggles.radio Msg.Mdl
                [ id ]
                model.mdl
                [ Toggles.ripple
                , Toggles.group "searchSelect"
                , Toggles.value (searchType == model.searchType)
                , Options.onToggle (Msg.ChangeSearchType searchType)
                , Options.cs "darn-search-box__search-type"
                ]
                [ text name ]
    in
        options |> List.map toRadio


searchInput : Model -> Html Msg
searchInput model =
    Textfield.render Msg.Mdl
        [ 0 ]
        model.mdl
        [ Textfield.label "Search"
        , Textfield.value model.query
        , Options.onInput Msg.QueryInput
        , Options.cs "darn-search-box__input"
        ]
        []


searchBox : Model -> Html Msg
searchBox model =
    Html.form [ onSubmit Msg.QuerySubmit, class "darn-search-box__form" ]
        [ div [ class "flex-container flex-container--center" ]
            [ searchInput model
            , Button.render Msg.Mdl
                [ 0 ]
                model.mdl
                [ Button.minifab
                , Button.ripple
                , Button.colored
                , Options.cs "flex-container__item"
                ]
                [ Icon.i "search" ]
            ]
        , searchSelect model
            |> List.map List.singleton
            |> List.map (div [ class "flex-container__item" ])
            |> div [ class "flex-container" ]
        , hr [] []
        ]


renderItems : Model -> Html Msg
renderItems model =
    case model.items of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            Options.div
                [ Options.cs "darn-center" ]
                [ Options.div [ Options.cs "darn-spinner" ]
                    [ Spinner.spinner [ Spinner.active True ]
                    , Options.div [ Options.cs "darn-center--text" ] [ text "Loading..." ]
                    ]
                ]

        RemoteData.Failure e ->
            Options.div
                [ Options.cs "darn-center" ]
                [ Options.div []
                    [ Options.div [] [ text ("Error: " ++ toString e) ]
                    , Button.render Msg.Mdl
                        [ 1 ]
                        model.mdl
                        [ Button.ripple, Button.raised, Options.onClick Msg.RetryRequest ]
                        [ text "Retry" ]
                    ]
                ]

        RemoteData.Success (Model.PaginatedSongs page) ->
            renderSongPage model.route page

        RemoteData.Success (Model.PaginatedArtists page) ->
            page.items |> List.map renderArtist |> mainGrid

        RemoteData.Success (Model.PaginatedSeries page) ->
            let
                categoryId =
                    Maybe.withDefault defaultSeriesCategoryId page.seriesCategoryId
            in
                page.items |> List.map (renderSeries categoryId) |> mainGrid

        RemoteData.Success (Model.PaginatedCategoryGroups page) ->
            page.items |> List.map renderCategoryGroup |> div []


mainGrid : List (Html Msg) -> Html Msg
mainGrid items =
    if List.isEmpty items then
        div [ class "darn-center" ]
            [ Icon.view "sentiment_very_dissatisfied"
                [ Icon.size48, Color.color Color.Grey Color.S500 |> Color.text ]
            , div [] [ text "Nothing found" ]
            ]
    else
        items
            |> List.map (List.singleton >> Grid.cell [ Grid.size Grid.All 4 ])
            |> Grid.grid [ Options.cs "darn-main-content__grid" ]


centeredColumn : List (Html Msg) -> Html Msg
centeredColumn contents =
    Grid.grid
        [ Options.cs "darn-main-content__grid"
        , Options.cs "darn-main-content__grid--centered"
        ]
        [ Grid.cell [ Grid.size Grid.All 4 ] contents ]


renderItem : List (Attribute Msg) -> List (Html Msg) -> Html Msg
renderItem attributes innerContents =
    a
        (class "darn-search-item__link" :: attributes)
        [ Options.div
            [ Elevation.e4
            , Options.cs "darn-search-item__container"
            ]
            innerContents
        ]


renderSongItem : Model.Song -> List (Html Msg) -> Html Msg
renderSongItem song innerContents =
    Options.div
        [ Options.cs "darn-search-item__link"
        , Options.onClick (Msg.ShowSong (Just song))
        ]
        [ Options.div
            [ Elevation.e4
            , Options.cs "darn-search-item__container"
            ]
            innerContents
        ]


renderRouteItem : Route -> List (Html Msg) -> Html Msg
renderRouteItem onClickRoute =
    renderItem [ onClickRoute |> Route.reverse |> href ]


renderSongPage : Route -> Model.Paginated Model.Song -> Html Msg
renderSongPage route page =
    case route of
        Route.SongInfo songId ->
            centeredColumn
                [ itemsHeader (displaySongId songId)
                , List.head page.items
                    |> Util.maybeFold (renderSongDialogContents page.seriesCategoryId) (text "")
                , a
                    [ Route.reverse (Route.SimilarSongs songId) |> href ]
                    [ button
                        [ class "mdl-button mdl-button--raised" ]
                        [ text "Find similar songs" ]
                    ]
                ]

        Route.ArtistSongs artistId ->
            div []
                [ page.items
                    |> List.head
                    |> Maybe.map (.artist >> .name)
                    |> Util.orEmptyString
                    |> itemsHeader
                , page.items |> List.map renderArtistSong |> mainGrid
                ]

        Route.SeriesSongs categoryId seriesTitle ->
            div []
                [ Http.decodeUri seriesTitle |> Util.orEmptyString |> itemsHeader
                , page.items |> List.map renderSong |> mainGrid
                ]

        Route.CategorySongs categoryId ->
            -- Sorry about the hardcoded values
            if categoryId >= "030000" && categoryId < "040000" then
                div [] (renderRecentSongs page)
            else
                page.items |> List.map renderCategorySong |> mainGrid

        _ ->
            page.items |> List.map renderSong |> mainGrid


renderArtistSong : Model.Song -> Html Msg
renderArtistSong song =
    renderSongItem song [ itemTitle True song.title ]


renderRecentSongs : Model.Paginated Model.Song -> List (Html Msg)
renderRecentSongs page =
    let
        sortField s =
            Util.orEmptyString s.dateAdded

        groupedByDate =
            page.items
                |> Util.groupBy sortField
                |> Dict.toList
                |> List.reverse

        groupToHtml : ( String, List Model.Song ) -> List (Html Msg)
        groupToHtml ( date, songs ) =
            [ itemsHeader date
            , songs |> List.map renderCategorySong |> mainGrid
            ]
    in
        List.concatMap groupToHtml groupedByDate


renderCategorySong : Model.Song -> Html Msg
renderCategorySong song =
    renderSongItem song
        [ itemTitle False song.title
        , song.series |> Maybe.withDefault song.artist.name |> itemSubtitle
        ]


itemTitle : Bool -> String -> Html Msg
itemTitle isLarge string =
    let
        suffix =
            if isLarge then
                "--large"
            else
                ""

        class =
            "darn-search-item__title" ++ suffix
    in
        Options.div [ Options.cs class ] [ text string ]


itemSubtitle : String -> Html Msg
itemSubtitle string =
    Options.div
        [ Options.cs "darn-search-item__subtitle" ]
        [ text string ]


itemsHeader : String -> Html Msg
itemsHeader string =
    Options.div
        [ Options.cs "darn-search-header" ]
        [ text string ]


itemIcon : String -> Html Msg
itemIcon icon =
    i [ class "material-icons", class "darn-search-item__icon" ] [ text icon ]


renderSong : Model.Song -> Html Msg
renderSong song =
    renderSongItem song
        [ itemTitle False song.title
        , itemSubtitle song.artist.name
        ]


renderArtist : Model.Artist -> Html Msg
renderArtist artist =
    renderRouteItem (Route.ArtistSongs artist.id) [ itemTitle True artist.name ]


renderSeries : String -> Model.Series -> Html Msg
renderSeries categoryId series =
    renderRouteItem
        (Route.SeriesSongs categoryId series.title)
        [ itemTitle True series.title ]


renderCategoryGroup : Model.CategoryGroup -> Html Msg
renderCategoryGroup categoryGroup =
    div []
        [ itemsHeader categoryGroup.description.en
        , categoryGroup.categories
            |> List.sortBy (.description >> .en)
            |> List.map renderCategory
            |> mainGrid
        ]


renderCategory : Model.Category -> Html Msg
renderCategory category =
    renderRouteItem (Route.CategorySongs category.id)
        [ itemTitle False category.description.en
        , itemSubtitle category.description.ja
        ]


renderSettings : Model -> Html Msg
renderSettings model =
    let
        settings =
            model.settings

        listItem : List (Html Msg) -> List (Html Msg) -> Html Msg
        listItem contents secondaryContents =
            MdlList.li [ Options.cs "darn-settings__list-item" ]
                [ MdlList.content [] contents
                , MdlList.content2 [] secondaryContents
                ]

        machineToRadio : Int -> Model.KaraokeMachine -> Html Msg
        machineToRadio index machine =
            Toggles.radio Msg.Mdl
                [ 2, index ]
                model.mdl
                [ Toggles.value (settings.serialNo == machine.serialNo)
                , Options.onToggle <| Msg.UpdateSettings { settings | serialNo = machine.serialNo }
                ]
                [ text machine.name ]

        machineToggles =
            Model.karaokeMachines
                |> List.indexedMap machineToRadio
                |> List.map (List.singleton >> li [])
    in
        MdlList.ul []
            [ listItem
                [ text "Machine" ]
                [ MdlList.ul [] machineToggles ]
            ]
