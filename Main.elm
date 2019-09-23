module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Json.Decode as D exposing (Decoder, field, string)

import Html exposing (Html, div, text, br)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill)
import Time exposing (Posix, every, posixToMillis)
import Task exposing (perform)

type alias Model =
    { pressedKey : String
    , score : Lane -> List Notes
    , startTime : Int
    , spentTime : Int
    , bpm : Int
    , isPressed :
        { leftLane : Bool
        , middleLeftLane : Bool
        , middleRightLane : Bool
        , rightLane : Bool
        }
    }

initialModel : Model
initialModel =
    { pressedKey = ""
    , score = initialScore
    , startTime = 0
    , spentTime = 1000
    , bpm = 120
    , isPressed =
        { leftLane = False
        , middleLeftLane = False
        , middleRightLane = False
        , rightLane = False
        }
    }

initialScore lane =
    case lane of
        Left        -> [Tap 120, Tap 480, Tap 1200]
        MiddleLeft  -> [Tap 240, Tap 600, Tap 1200]
        MiddleRight -> [Tap 360, Tap 720, Tap 960 ]
        Right       -> [Tap 480, Tap 840, Tap 1080]
        None        -> []

type Msg
    = Pressed Lane
    | Released Lane
    | Tick Posix
    | Start Posix

type Lane
    = Left
    | MiddleLeft
    | MiddleRight
    | Right
    | None

type Notes
    = Tap Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pressed lane ->
            model
                |> setLaneState lane True

        Released lane ->
            model
                |> setLaneState lane False

        Tick posix ->
            ( { model | spentTime = (posixToMillis posix) - model.startTime}
            , Cmd.none
            )

        Start posix ->
            ( { model | startTime = posixToMillis posix }
            , Cmd.none
            )

setLaneState: Lane -> Bool -> Model -> ( Model, Cmd Msg )
setLaneState lane bool model =
    let
        oldIsPressed = model.isPressed
        newIsPressed =
            case lane of
                Left        -> { oldIsPressed | leftLane        = bool }
                MiddleLeft  -> { oldIsPressed | middleLeftLane  = bool }
                MiddleRight -> { oldIsPressed | middleRightLane = bool }
                Right       -> { oldIsPressed | rightLane       = bool }
                None        -> oldIsPressed
    in
        ( { model | isPressed = newIsPressed }, Cmd.none )

view : Model -> Html Msg
view model =
    let
        laneWidth lane=
            case lane of
                Left        -> "0"
                MiddleLeft  -> "50"
                MiddleRight -> "100"
                Right       -> "150"
                None        -> "0"

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
                            None        -> "white"
                            )
                        ]
                        []

        drawNotes =
            [ Left, MiddleLeft, MiddleRight, Right ]
                |> List.concatMap (\lane -> model.score lane
                    |> List.map (\notes -> drawNote lane notes model.spentTime))

        changeLaneColor =
            List.indexedMap
                (\n pressed ->
                    rect
                        [ x (String.fromInt (n*50))
                        , y "0"
                        , width "50"
                        , height "120"
                        , fill (if pressed then "orange" else "white")
                        ]
                        []
                )
                [ model.isPressed.leftLane
                , model.isPressed.middleLeftLane
                , model.isPressed.middleRightLane
                , model.isPressed.rightLane
                ]


    in
        div[]
            [ svg[]
                <| List.append changeLaneColor
                <| List.append drawNotes
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

stringToLane : String -> Lane
stringToLane str=
    case str of
        "f" -> Left
        "g" -> MiddleLeft
        "h" -> MiddleRight
        "j" -> Right
        _   -> None

keyDownDecoder =
    (field "key" string)
        |> D.map stringToLane
        |> D.map Pressed

keyUpDecoder =
    (field "key" string)
        |> D.map stringToLane
        |> D.map Released

subscription : Model -> Sub Msg
subscription model =
    Sub.batch
        [ onKeyDown keyDownDecoder
        , onKeyUp keyUpDecoder
        , Time.every 30 Tick
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_->(initialModel, perform Start Time.now )
        , view = view
        , update = update
        , subscriptions = subscription
        }
