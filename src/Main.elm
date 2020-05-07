module Main exposing (..)

import Browser
import Html exposing (div, table, td, text, tr)
import Html.Attributes
import Html.Events
import List exposing (filter, head, member)
import Matrix exposing (..)
import Random
import String exposing (fromInt)
import Tuple exposing (first, second)


type alias Model =
    Matrix Int


type Msg
    = Click Int Int
    | Shuffle (List Int)
    | ShuffleButton


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscribe
        }


init =
    ( Matrix.initialize 4 4 (\r c -> r * 4 + c), Cmd.none )


update msg model =
    case msg of
        Click row col ->
            onClickUpdate row col model

        Shuffle lst ->
            onShuffleUpdate lst model

        ShuffleButton ->
            ( model, shuffleCmd )


subscribe model =
    Sub.none


shuffleCmd =
    Random.generate Shuffle (Random.list 1000 (Random.int 1 4))


onClickUpdate fromRow fromCol model =
    let
        ( toRow, toCol ) =
            findZero model

        options =
            moves model
    in
    if member ( fromRow, fromCol ) options then
        ( swap ( fromRow, fromCol ) ( toRow, toCol ) model, Cmd.none )

    else
        ( model, Cmd.none )


onShuffleUpdate lst model =
    let
        zero =
            findZero model

        options =
            moves model
    in
    case lst of
        [] ->
            ( model, Cmd.none )

        a :: rest ->
            onShuffleUpdate rest (swap zero (head (rotate a options)) model)


rotate n lst =
    if 0 < n then
        case lst of
            [] ->
                []

            a :: rest ->
                rotate (n - 1) (rest ++ [ a ])

    else
        lst


findZero model =
    head (filter (\e -> 0 == get (first e) (second e) model) (iterator 0 3 0 3))


iterator topRow bottomRow leftCol rightCol =
    let
        makeRow n =
            List.map (Tuple.pair n) (List.range leftCol rightCol)
    in
    List.concatMap makeRow (List.range topRow bottomRow)


swap a b model =
    let
        aValue =
            get (first a) (second a) model

        bValue =
            get (first b) (second b) model
    in
    model
        |> set (first b) (second b) aValue
        |> set (first a) (second a) bValue


moves model =
    let
        ( row, col ) =
            findZero model

        options =
            [ ( row - 1, col ), ( row + 1, col ), ( row, col - 1 ), ( row, col + 1 ) ]
    in
    filter (\( r, c ) -> 0 <= r && r < 4 && 0 <= c && c < 4) options


head lst =
    Maybe.withDefault ( -10, -10 ) (List.head lst)


get row col m =
    Maybe.withDefault -1 (Matrix.get row col m)



-- VIEW


view model =
    div
        [ Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "font-size" "24pt"
        , Html.Attributes.style "margin" "2em"
        ]
        [ div
            [ Html.Attributes.style "font-family" "sans-serif"
            , Html.Attributes.style "margin" "2em"
            ]
            [ text "Sliding Puzzle" ]
        , viewPuzzle model
        , Html.button
            [ Html.Attributes.style "background-color" "mediumseagreen"
            , Html.Attributes.style "font-size" "24pt"
            , Html.Attributes.style "margin" "2em"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "border-radius" "12px"
            , Html.Attributes.style "padding" "10px"
            , Html.Events.onClick ShuffleButton
            ]
            [ Html.text "Shuffle" ]
        ]


viewPuzzle model =
    let
        viewSq row col sq =
            let
                color =
                    if 0 == sq then
                        "lightgrey"

                    else
                        "darkcyan"
            in
            td
                [ Html.Attributes.style "border" "1px solid lightgrey"
                , Html.Attributes.style "width" "80px"
                , Html.Attributes.style "height" "80px"
                , Html.Attributes.style "background" color
                , Html.Attributes.style "border-radius" "8px"
                , Html.Attributes.style "font-size" "24pt"
                , Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "color" "lightgrey"
                , Html.Events.onClick (Click row col)
                ]
                [ text (fromInt sq) ]

        viewRow row =
            tr [] (List.map (\col -> viewSq row col (get row col model)) (List.range 0 3))
    in
    table
        [ Html.Attributes.style "border-collapse" "collapse"
        , Html.Attributes.style "margin" "auto"
        , Html.Attributes.style "padding" "2em"
        , Html.Attributes.style "box-shadow" "5px 5px 3px grey"
        , Html.Attributes.style "border" "5px solid black"
        , Html.Attributes.style "background" "lightgrey"
        ]
        (List.map (\e -> viewRow e) (List.range 0 3))
