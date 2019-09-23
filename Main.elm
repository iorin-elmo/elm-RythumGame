module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Json.Decode as D exposing (Decoder, field, string, keyValuePairs)

import Html exposing (Html, div, text, br)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill)
import Time exposing (Posix, every, posixToMillis)
import Task exposing (perform)

type alias Model =
    {pressedKey : String, score : Lane -> List Notes, startTime : Int, spentTime : Int, bpm : Int}

initialModel : Model
initialModel =
    { pressedKey = ""
    , score = initialScore
    , startTime = 0
    , spentTime = 1000
    , bpm = 120
    }

initialScore lane =
    case lane of
        Left        -> [Tap 120, Tap 480, Tap 1200]
        MiddleLeft  -> [Tap 240, Tap 600, Tap 1200]
        MiddleRight -> [Tap 360, Tap 720, Tap 960 ]
        Right       -> [Tap 480, Tap 840, Tap 1080]

type Msg
    = Pressed Lane
    | Tick Posix
    | Start Posix

type Lane
    = Left
    | MiddleLeft
    | MiddleRight
    | Right

type Notes
    = Tap Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pressed lane ->
            ( model
            , Cmd.none
            )

        Tick posix ->
            ( { model | spentTime = (posixToMillis posix) - model.startTime}
            , Cmd.none
            )

        Start posix ->
            ( { model | startTime = posixToMillis posix }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    let
        laneWidth lane=
            case lane of
                Left        -> "0"
                MiddleLeft  -> "50"
                MiddleRight -> "100"
                Right       -> "150"

        drawNote : Lane -> Notes -> Int -> Svg Msg
        drawNote lane notes time =
            case notes of
                Tap n ->
                    rect
                        [ x (laneWidth lane)
                        , y (String.fromInt (time//20-n//model.bpm*10))
                        , width "50"
                        , height "5"
                        , fill (case lane of
                            Left        -> "Blue"
                            MiddleLeft  -> "Red"
                            MiddleRight -> "Red"
                            Right       -> "Blue"
                            )
                        ]
                        []

        drawNotes =
            [ Left, MiddleLeft, MiddleRight, Right ]
                |> List.concatMap (\lane -> model.score lane
                    |> List.map (\notes -> drawNote lane notes model.spentTime))

    in
        div[]
            [ svg[]
                <| List.append
                    drawNotes
                    [ rect
                        [ x "0"
                        , y "120"
                        , width "200"
                        , height "2"
                        , fill "black"
                        ]
                        []
                    ]
            ]

{-
subscription : Model -> Sub Msg
subscription model =
    onKeyDown <| D.map Pressed forceStringDecoder

forceStringDecoder : Decoder String
forceStringDecoder =
    D.oneOf
        [ D.string
            |> D.andThen (\str -> D.succeed str)
        , D.int
            |> D.andThen (\num -> D.succeed (String.fromInt num))
        , D.float
            |> D.andThen (\num -> D.succeed (String.fromFloat num))
        , D.bool
            |> D.andThen (\bool -> D.succeed (if bool then "True" else "False"))
        , D.lazy
            (\_ -> D.list forceStringDecoder)
            |> D.andThen (\_ -> D.succeed "gave up")
        , D.lazy
            (\_ ->
                D.keyValuePairs forceStringDecoder
            )
            |> D.andThen ( List.map tupleToString
                >> List.intersperse "\n"
                >> String.concat
                >> D.succeed )
        , D.nullable D.value
            |> D.andThen (\_ -> D.succeed "Null")
        ]

tupleToString : (String, String) -> String
tupleToString (n,m)=
    "(" ++ n ++ " : " ++ m ++ ")"

-}
main : Program () Model Msg
main =
    Browser.element
        { init = \_->(initialModel, perform Start Time.now )
        , view = view
        , update = update
        , subscriptions =\_ -> Time.every 30 Tick
        }
