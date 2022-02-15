module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyUp)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Random
import Words



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Cell
    = Uncommitted Char
    | Absent Char
    | Present Char
    | Correct Char


type alias Row =
    Array Cell


type alias Board =
    Array Row


type alias Cursor =
    ( Int, Int )


type alias Model =
    { word : String
    , board : Board
    , cursor : Cursor
    , hasWon : Bool
    }


createBoard : Board
createBoard =
    let
        blankCell =
            Uncommitted ' '

        blankRow =
            Array.fromList <| List.repeat 5 blankCell
    in
    Array.fromList <| List.repeat 6 blankRow


defaultWord : String
defaultWord =
    "stare"


todaysWord : Int -> String
todaysWord idx =
    let
        candidates =
            Words.asList |> Array.fromList

        possibleWord =
            Array.get idx candidates
    in
    case possibleWord of
        Nothing ->
            defaultWord

        Just word ->
            word


initialModel : Model
initialModel =
    { word = defaultWord
    , board = createBoard
    , cursor = ( 0, 0 )
    , hasWon = False
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Random.generate TodaysWordByIndex (Random.int 0 Words.length) )



-- SUBSCRIPTIONS
--
-- Adapted from:
-- https://elmprogramming.com/subscriptions.html#subscribing-to-a-specific-key-event


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        _ ->
            ControlKey keyValue


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyUp keyDecoder



-- UPDATE


type Msg
    = CharacterKey Char
    | ControlKey String
    | TodaysWordByIndex Int


setLetterAt : Cursor -> Board -> Char -> Board
setLetterAt ( rowIndex, columnIndex ) board character =
    let
        letter =
            Uncommitted character

        row =
            Array.get rowIndex board
    in
    case row of
        Just cells ->
            Array.set rowIndex (Array.set columnIndex letter cells) board

        Nothing ->
            Array.empty


advanceColumnBy : Int -> Cursor -> Cursor
advanceColumnBy value ( row, column ) =
    -- TODO: Ensure we don't fall off either end of the row
    -- (it doesn't break the experience, but is confusing
    -- since the cursor is effectively "off the screen")
    ( row, column + value )


advanceRow : Model -> Cursor
advanceRow model =
    let
        ( row, _ ) =
            model.cursor

        boardSize =
            Array.length model.board - 1
    in
    if row < boardSize then
        ( row + 1, 0 )

    else
        ( row, 0 )


checkIfWon : Model -> Bool
checkIfWon model =
    let
        board =
            model.board

        ( rowIndex, _ ) =
            model.cursor

        row =
            Array.get rowIndex board

        word =
            model.word
    in
    case row of
        Just cells ->
            word
                == (cells
                        |> Array.toList
                        |> List.map extractChar
                        |> String.fromList
                   )

        Nothing ->
            False


updateCell : Int -> ( Cell, String ) -> Cell
updateCell index ( cell, word ) =
    let
        chars =
            word |> String.toList

        isIn =
            List.member

        isCorrectlyPlacedIn =
            \char w -> isIn char w && isIn index (indicesOf char chars)
    in
    case cell of
        Uncommitted character ->
            if isCorrectlyPlacedIn character chars then
                Correct character

            else if isIn character chars then
                Present character

            else
                Absent character

        _ ->
            cell


updateRow : Row -> String -> Row
updateRow row word =
    let
        decoratedRow =
            Array.map (\cell -> ( cell, word )) row
    in
    Array.indexedMap updateCell decoratedRow


commitGuess : Model -> Board
commitGuess model =
    let
        word =
            model.word

        board =
            model.board

        cursor =
            model.cursor

        ( rowIndex, _ ) =
            cursor

        row =
            Array.get rowIndex board
    in
    case row of
        Just cells ->
            Array.set rowIndex (updateRow cells word) board

        Nothing ->
            Array.empty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CharacterKey charValue ->
            ( { model
                | board =
                    setLetterAt model.cursor model.board charValue
                , cursor =
                    advanceColumnBy 1 model.cursor
              }
            , Cmd.none
            )

        -- TODO: Explicitly handle loss condition
        -- instead of just ending the game
        ControlKey "Enter" ->
            ( { model
                | board = commitGuess model
                , cursor = advanceRow model
                , hasWon = checkIfWon model
              }
            , Cmd.none
            )

        ControlKey "Backspace" ->
            ( { model
                | board =
                    setLetterAt (advanceColumnBy -1 model.cursor) model.board ' '
                , cursor =
                    advanceColumnBy -1 model.cursor
              }
            , Cmd.none
            )

        TodaysWordByIndex randomIndex ->
            ( { model | word = todaysWord randomIndex }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


toSquare : Cell -> Html a
toSquare cell =
    case cell of
        Uncommitted char ->
            -- TODO: Swap this out for an actual CSS
            -- file (or maybe just Elm UI instead?)
            div
                [ style "background-color" "white"
                , style "border" "2px solid #d3d6da"
                , style "border-radius" "5%"
                , style "display" "inline-flex"
                , style "flex-direction" "column"
                , style "font-family" "helvetica"
                , style "height" "4em"
                , style "justify-content" "center"
                , style "margin" "0 0.25em 0.25em 0"
                , style "text-align" "center"
                , style "vertical-align" "top"
                , style "width" "4em"
                ]
                [ text (String.fromChar char |> String.toUpper) ]

        Absent char ->
            div
                [ style "background-color" "#787c7e"
                , style "border" "2px solid #d3d6da"
                , style "border-radius" "5%"
                , style "color" "white"
                , style "display" "inline-flex"
                , style "flex-direction" "column"
                , style "font-family" "helvetica"
                , style "height" "4em"
                , style "justify-content" "center"
                , style "margin" "0 0.25em 0.25em 0"
                , style "text-align" "center"
                , style "vertical-align" "top"
                , style "width" "4em"
                ]
                [ text (String.fromChar char |> String.toUpper) ]

        Present char ->
            div
                [ style "background-color" "#c9b458"
                , style "border" "2px solid #d3d6da"
                , style "border-radius" "5%"
                , style "color" "white"
                , style "display" "inline-flex"
                , style "flex-direction" "column"
                , style "font-family" "helvetica"
                , style "height" "4em"
                , style "justify-content" "center"
                , style "margin" "0 0.25em 0.25em 0"
                , style "text-align" "center"
                , style "vertical-align" "top"
                , style "width" "4em"
                ]
                [ text (String.fromChar char |> String.toUpper) ]

        Correct char ->
            div
                [ style "background-color" "#6aaa64"
                , style "border" "2px solid #d3d6da"
                , style "border-radius" "5%"
                , style "color" "white"
                , style "display" "inline-flex"
                , style "flex-direction" "column"
                , style "font-family" "helvetica"
                , style "height" "4em"
                , style "justify-content" "center"
                , style "margin" "0 0.25em 0.25em 0"
                , style "text-align" "center"
                , style "vertical-align" "top"
                , style "width" "4em"
                ]
                [ text (String.fromChar char |> String.toUpper) ]


fromRow : Row -> Html Msg
fromRow guess =
    div []
        (List.map toSquare (Array.toList guess))


display : Board -> List (Html Msg)
display board =
    List.map fromRow (Array.toList board)


showGameResult : Model -> String
showGameResult model =
    let
        ( numberOfGuesses, _ ) =
            model.cursor
    in
    if model.hasWon then
        -- TODO: Fix bug where cursor doesn't properly
        -- advance for a win on the sixth move
        "Got it in " ++ String.fromInt numberOfGuesses ++ "!"

    else
        ""


view : Model -> Html Msg
view model =
    div []
        [ h1
            [ style "text-align" "center"
            , style "font-family" "helvetica"
            ]
            [ text "WORSLE" ]
        , h2
            [ style "text-align" "center"
            , style "font-family" "helvetica"
            , style "font-size" "1em"
            ]
            [ text "Wordle, just... slightly worse" ]
        , div
            [ style "margin" "0 auto"
            , style "width" "22.5em"
            ]
            (display model.board)
        , p
            [ style "text-align" "center"
            , style "font-family" "helvetica"
            ]
            [ text (showGameResult model) ]
        ]



-- HELPERS


extractChar : Cell -> Char
extractChar cell =
    case cell of
        Uncommitted char ->
            char

        Absent char ->
            char

        Present char ->
            char

        Correct char ->
            char


indicesOf : a -> List a -> List Int
indicesOf desired list =
    list
        |> List.indexedMap Tuple.pair
        |> List.filter (\( idx, item ) -> item == desired)
        |> List.map Tuple.first
