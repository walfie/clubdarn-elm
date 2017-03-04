module ClubDarn.View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import ClubDarn.Msg as Msgs exposing (Msg)
import ClubDarn.Model exposing (Model)
import ClubDarn.Route as Route exposing (Route)


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
        Route.SearchResults (Route.SongSearch) query ->
            text ("Song search " ++ (Maybe.withDefault "" query))

        Route.SearchResults (Route.ArtistSearch) query ->
            text ("Artist search " ++ (Maybe.withDefault "" query))

        Route.SearchResults (Route.SeriesSearch) query ->
            text ("Series search " ++ (Maybe.withDefault "" query))

        Route.CategoryListing ->
            text "Categories"

        _ ->
            text "Other"
