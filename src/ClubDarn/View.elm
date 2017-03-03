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
        [ searchBox model
        , navigation
        , mainContent model
        ]


searchBox : Model -> Html Msg
searchBox model =
    Html.form [ onSubmit Msgs.QuerySubmit ]
        [ input [ onInput Msgs.QueryInput, value model.query ] []
        , button [] [ text "Search" ]
        ]


navigation : Html Msg
navigation =
    ul []
        ([ ( "Songs", Route.SongSearch Nothing )
         , ( "Artists", Route.ArtistSearch Nothing )
         , ( "Series", Route.SeriesSearch Nothing )
         , ( "Categories", Route.CategoryListing )
         ]
            |> List.map (\( name, route ) -> a [ route |> Route.reverse |> href ] [ text name ])
            |> List.map (\link -> li [] [ link ])
        )


mainContent : Model -> Html Msg
mainContent model =
    case model.route of
        Route.SongSearch query ->
            text ("Song search " ++ (Maybe.withDefault "" query))

        Route.ArtistSearch query ->
            text ("Artist search " ++ (Maybe.withDefault "" query))

        Route.SeriesSearch query ->
            text ("Series search " ++ (Maybe.withDefault "" query))

        Route.CategoryListing ->
            text "Categories"

        _ ->
            text "Other"
