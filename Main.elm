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

import Lane exposing (Lane(..), LaneMap)

type alias Model =
    { pressedKey   : String
    , score        : LaneMap (List Notes)
    , visibleNotes : LaneMap (List Notes)
    , startTime : Int
    , spentTime : Int
    , bpm       : Int
    , isPressed : LaneMap Bool
    , speed : Int
    , visibleEvaluation : Evaluation
    , grades :
        { criticalPerfect : Int
        , perfect : Int
        , great : Int
        , good : Int
        , miss : Int
        }
    }

initialModel : Model
initialModel =
    { pressedKey   = ""
    , score        = initialScore
    , visibleNotes = Lane.fill []
    , startTime = 0        --[ms]
    , spentTime = 1000     --[ms]
    , bpm       = 120      --[beats/min]
    , isPressed = Lane.fill False
    , speed = 0
    , visibleEvaluation = TooFar
    , grades =
        { criticalPerfect = 0
        , perfect = 0
        , great = 0
        , good = 0
        , miss = 0
        }
    }

type Notes
    = Tap Int             --[beatUnit]
    | Hold Int Int        --[beatUnit]

beatUnit = 120 -- [ / beat]

initialScore =
    Lane.fill []
        |> Lane.put Left (
            [Tap 120, Tap 480, Tap 1200] ++
            [Hold 1320 1800])
        |> Lane.put MiddleLeft (
            [Tap 240, Tap 600, Tap 1200] ++
            [Hold 1800 3000])
        |> Lane.put MiddleRight (
            [Tap 360, Tap 720, Tap 960 ] ++
            [Hold 3600 5400])
        |> Lane.put Right (
            [Tap 480, Tap 840, Tap 1080] ++
            [Hold 5400 7200])

type Msg
    = Pressed Lane
    | Released Lane
    | Tick Posix
    | Start Posix
    | None

type Evaluation
    = CriticalPerfect
    | Perfect
    | Great
    | Good
    | Miss
    | TooFar

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pressed lane ->
            ( model
                |> setLaneState lane True
                |> evaluate lane
            , Cmd.none
            )

        Released lane ->
            ( model
                |> setLaneState lane False
                |> evaluate lane
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

        None ->
            ( model
            , Cmd.none
            )

setLaneState: Lane -> Bool -> Model -> Model
setLaneState lane bool model =
    { model | isPressed = model.isPressed |> Lane.put lane bool }

evaluate : Lane -> Model -> Model
evaluate lane model =
    case lane of
        Left -> model
        MiddleLeft->model
        MiddleRight->model
        Right->model

view : Model -> Html Msg
view model =
    let
        spentTimeInBeats = model.spentTime * model.bpm // 60 * beatUnit // 1000

        drawNote : Lane -> Notes -> Int -> List (Svg Msg)
        drawNote lane notes time =
            case notes of
                Tap n ->
                    drawTap lane (n - spentTimeInBeats)

                Hold start end ->
                    drawHold lane (start - spentTimeInBeats) (end - spentTimeInBeats)

        drawNotes =
            [ Left, MiddleLeft, MiddleRight, Right ]
                |> List.concatMap (\lane ->
                    let laneScore = model.score |> Lane.get lane in
                    laneScore
                        |> List.concatMap (\notes -> drawNote lane notes model.spentTime))

        changeLaneColor =
            model.isPressed
                |> Lane.toList
                |> List.map
                    (\(lane, pressed) ->
                        rect
                            [ x (lane |> lanePosition |> String.fromInt)
                            , y "0"
                            , width "50"
                            , height (judgeLine |> String.fromInt)
                            , fill (if pressed then "orange" else "white")
                            ]
                            []
                    )


    in
        div[]
            [ svg[]
                <| List.append changeLaneColor
                <| List.append drawNotes
                    [ rect
                        [ x "0"
                        , y (judgeLine |> String.fromInt)
                        , width "200"
                        , height "2"
                        , fill "black"
                        ]
                        []
                    ]
            ]

pixelPerBeatUnit = 1 -- [px / beatUnit]
judgeLine = 120 -- [px]

lanePosition lane = -- [px]
    case lane of
        Left        -> 0
        MiddleLeft  -> 50
        MiddleRight -> 100
        Right       -> 150

laneColor lane =
    case lane of
        Left        -> "Blue"
        MiddleLeft  -> "Red"
        MiddleRight -> "Red"
        Right       -> "Blue"

drawTap lane relativeBeats = -- レーン，判定線までの残り [beatUnit]
    [ rect
        [ x <| String.fromInt <| lanePosition lane
        , y <| String.fromInt (judgeLine - (relativeBeats * pixelPerBeatUnit))
        , width "50"
        , height "5"
        , fill <| laneColor lane
        ]
        []
    ]

drawHold lane start end =
    [ rect
        [ x <| String.fromInt <| lanePosition lane
        , y <| String.fromInt <| (judgeLine - (start * pixelPerBeatUnit))
        , width "50"
        , height "5"
        , fill <| laneColor lane
        ]
        []
        , rect
        [ x <| String.fromInt <| lanePosition lane
        , y <| String.fromInt <| (judgeLine - (end * pixelPerBeatUnit))
        , width "50"
        , height "5"
        , fill <| laneColor lane
        ]
        []
        , rect
        [ x <| String.fromInt <| (lanePosition lane) + 5
        , y <| String.fromInt <| (judgeLine - (end * pixelPerBeatUnit))
        , width "40"
        , height <| String.fromInt <| (end - start) * pixelPerBeatUnit
        , fill <| laneColor lane
        ]
        []
    ]


stringToLane : String -> Maybe Lane
stringToLane str =
    case str of
        "f" -> Just Left
        "g" -> Just MiddleLeft
        "h" -> Just MiddleRight
        "j" -> Just Right
        _   -> Nothing

keyDownDecoder =
    (field "key" string)
        |> D.map stringToLane
        |> D.map (Maybe.map Pressed >> Maybe.withDefault None)

keyUpDecoder =
    (field "key" string)
        |> D.map stringToLane
        |> D.map (Maybe.map Released >> Maybe.withDefault None)

subscription : Model -> Sub Msg
subscription model =
    Sub.batch
        [ onKeyDown keyDownDecoder
        , onKeyUp keyUpDecoder
        , Time.every 16 Tick
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_->(initialModel, perform Start Time.now )
        , view = view
        , update = update
        , subscriptions = subscription
        }