module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyUp)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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


type Key
    = Clicked String
    | Unclicked String


type alias Keyboard =
    List Key


type GameState
    = Playing
    | Won
    | Lost


type alias Model =
    { word : String
    , board : Board
    , keyboard : Keyboard
    , cursor : Cursor
    , guessCount : Int
    , gameState : GameState
    }


rowLength : Int
rowLength =
    5


boardLength : Int
boardLength =
    6


createBoard : Board
createBoard =
    let
        blankCell =
            Uncommitted ' '

        blankRow =
            Array.fromList <| List.repeat rowLength blankCell
    in
    Array.fromList <| List.repeat boardLength blankRow


createKeyboard : Keyboard
createKeyboard =
    let
        createKey =
            \letter -> Unclicked letter
    in
    List.map createKey
        [ "q"
        , "w"
        , "e"
        , "r"
        , "t"
        , "y"
        , "u"
        , "i"
        , "o"
        , "p"
        , "a"
        , "s"
        , "d"
        , "f"
        , "g"
        , "h"
        , "j"
        , "k"
        , "l"
        , "Enter"
        , "z"
        , "x"
        , "c"
        , "v"
        , "b"
        , "n"
        , "m"
        , "Backspace"
        ]


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
    Maybe.withDefault defaultWord possibleWord


initialModel : Model
initialModel =
    { word = defaultWord
    , board = createBoard
    , keyboard = createKeyboard
    , cursor = ( 0, 0 )
    , guessCount = 0
    , gameState = Playing
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
subscriptions model =
    case model.gameState of
        Playing ->
            onKeyUp keyDecoder

        _ ->
            Sub.none



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
    let
        newPosition =
            column + value
    in
    if newPosition < 0 then
        ( row, 0 )

    else if newPosition > rowLength then
        ( row, rowLength )

    else
        ( row, column + value )


advanceRow : Cursor -> Board -> Cursor
advanceRow cursor board =
    let
        ( row, _ ) =
            cursor

        boardSize =
            Array.length board
    in
    if row < boardSize then
        ( row + 1, 0 )

    else
        ( row, 0 )


updateGameState : Model -> GameState
updateGameState model =
    let
        board =
            model.board

        ( rowIndex, _ ) =
            model.cursor

        row =
            Array.get rowIndex board

        word =
            model.word

        hasGuessesLeft =
            model.guessCount < (boardLength - 1)

        correctGuess =
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
    in
    if correctGuess then
        Won

    else if hasGuessesLeft then
        Playing

    else
        Lost


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
        rowWithWordForComparison =
            Array.map (\cell -> ( cell, word )) row
    in
    Array.indexedMap updateCell rowWithWordForComparison



-- TODO: Prevent non-words from being submitted,
-- as well as prevent incomplete submissions


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


updateKey : ( Key, List String ) -> Key
updateKey ( key, guessedLetters ) =
    case key of
        Clicked _ ->
            key

        Unclicked letter ->
            if List.member letter guessedLetters then
                Clicked letter

            else
                key


updateKeyboard : Model -> Keyboard
updateKeyboard model =
    let
        board =
            model.board

        cursor =
            model.cursor

        ( rowIndex, _ ) =
            cursor

        row =
            Array.get rowIndex board

        characters =
            case row of
                Just cells ->
                    cells
                        |> Array.toList
                        |> List.map extractChar
                        |> List.map String.fromChar

                Nothing ->
                    []

        keyboard =
            model.keyboard

        keyboardWithLatestGuess =
            List.map (\key -> ( key, characters )) keyboard
    in
    List.map updateKey keyboardWithLatestGuess


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

        ControlKey "Enter" ->
            ( { model
                | board = commitGuess model
                , cursor = advanceRow model.cursor model.board
                , keyboard = updateKeyboard model
                , guessCount = model.guessCount + 1
                , gameState = updateGameState model
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


toSquare : Cell -> Html Msg
toSquare cell =
    -- TODO: Extract the `div` logic into its own function,
    -- parameterized by the class we wish to apply
    case cell of
        Uncommitted char ->
            div [ class "board-cell" ]
                [ text (String.fromChar char |> String.toUpper) ]

        Absent char ->
            div
                [ class "board-cell absent" ]
                [ text (String.fromChar char |> String.toUpper) ]

        Present char ->
            div
                [ class "board-cell present" ]
                [ text (String.fromChar char |> String.toUpper) ]

        Correct char ->
            div
                [ class "board-cell correct" ]
                [ text (String.fromChar char |> String.toUpper) ]


fromRow : Row -> Html Msg
fromRow guess =
    div []
        (List.map toSquare (Array.toList guess))


display : Model -> Html Msg
display model =
    let
        board =
            Array.toList model.board
    in
    case model.gameState of
        -- TODO: See comment for `toSquare`
        Won ->
            div [ class "victory" ]
                (List.map fromRow board)

        Lost ->
            div [ class "defeat" ]
                (List.map fromRow board)

        Playing ->
            div []
                (List.map fromRow board)



-- TODO: Conditionally include onclick depending on GameState


toKeyCap : Key -> Html Msg
toKeyCap keyPressed =
    case keyPressed of
        Unclicked letter ->
            div
                [ class "keycap"
                , onClick (toKey letter)
                ]
                [ text <| String.toUpper letter ]

        Clicked letter ->
            div
                [ class "keycap clicked"
                , onClick (toKey letter)
                ]
                [ text <| String.toUpper letter ]


renderKeyboard : Keyboard -> List (Html Msg)
renderKeyboard keyboard =
    [ div []
        (List.map toKeyCap keyboard)
    ]


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "WORSLE" ]
        , h2
            []
            [ text "Wordle, just... slightly worse" ]
        , div
            [ class "game-board" ]
            [ display model ]
        , div
            [ class "keyboard" ]
            (renderKeyboard model.keyboard)
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
