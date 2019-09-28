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
    { pressedKey   : String
    , score        : Lane -> List Notes
    , visibleNotes :
        { leftNotes        : List Notes
        , middleLeftNotes  : List Notes
        , middleRightNotes : List Notes
        , rightNotes       : List Notes
        }
    , startTime : Int
    , spentTime : Int
    , bpm       : Int
    , isPressed :
        { leftLane        : Bool
        , middleLeftLane  : Bool
        , middleRightLane : Bool
        , rightLane       : Bool
        }
    , speed : Int
    , grades : Evaluation -> Int
    }

initialModel : Model
initialModel =
    { pressedKey   = ""
    , score        = initialScore
    , visibleNotes =
        { leftNotes        = []
        , middleLeftNotes  = []
        , middleRightNotes = []
        , rightNotes       = []
        }
    , startTime = 0        --[ms]
    , spentTime = 1000     --[ms]
    , bpm       = 120      --[beats/min]
    , isPressed =
        { leftLane        = False
        , middleLeftLane  = False
        , middleRightLane = False
        , rightLane       = False
        }
    , speed = 0
    , grades = initialGrades
    }

type Notes
    = Tap Int             --[beats/120]
    | Hold Int Int        --[beats/120]

initialScore lane =
    case lane of
        Left        ->
            List.append
                [Tap 120, Tap 480, Tap 1200]
                [Hold 1320 1800]
        MiddleLeft  ->
            List.append
                [Tap 240, Tap 600, Tap 1200]
                [Hold 1800 3000]
        MiddleRight ->
            List.append
                [Tap 360, Tap 720, Tap 960 ]
                [Hold 3600 5400]
        Right       ->
            List.append
                [Tap 480, Tap 840, Tap 1080]
                [Hold 5400 7200]
        None        -> []

initialGrades evaluation =
    case evaluation of
        CriticalPerfect -> 0
        Perfect         -> 0
        Great           -> 0
        Good            -> 0
        Miss            -> 0
        TooFar          -> 0

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

type Evaluation
    = CriticalPerfect         --100%(+1)
    | Perfect                 --100%
    | Great                   -- 75%
    | Good                    -- 50%
    | Miss                    --  0%
    | TooFar

--Update--

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
--                |> setVisibleNotes
            , Cmd.none
            )

        Start posix ->
            ( { model | startTime = posixToMillis posix }
            , Cmd.none
            )

{-
setVisibleNotes : Lane -> Model -> Model
setVisibleNotes lane =
    case model. of
-}

setLaneState: Lane -> Bool -> Model -> Model
setLaneState lane bool model =
    let
        oldIsPressed = model.isPressed
        newIsPressed =
            case lane of
                Left        -> { oldIsPressed | leftLane        = bool }
                MiddleLeft  -> { oldIsPressed | middleLeftLane  = bool }
                MiddleRight -> { oldIsPressed | middleRightLane = bool }
                Right       -> { oldIsPressed | rightLane       = bool }
                None        ->   oldIsPressed
    in
        { model | isPressed = newIsPressed }

evaluate : Lane -> Model -> Model
evaluate lane model =
    let
        msToEvaluation : Int -> Notes -> Evaluation
        msToEvaluation pressedTime notes =
            case notes of
                Tap notesTime ->
                    let
                        timeRange=abs (notesTime - pressedTime)
                    in
                        if (timeRange<=33) then CriticalPerfect
                        else if (timeRange<=50) then Perfect
                        else if (timeRange<=100) then Great
                        else if (timeRange<=200) then Good
                        else if (timeRange<=400) then Miss
                        else TooFar
                Hold start end ->
                    TooFar
        allEvaluate=
            List.map (\_->msToEvaluation model.spentTime) (model.score lane)
    in
        model
        
--View--

view : Model -> Html Msg
view model =
    let
        laneWidth lane=
            case lane of
                Left        -> 0
                MiddleLeft  -> 50
                MiddleRight -> 100
                Right       -> 150
                None        -> 0

        notesColor lane =
            case lane of
                Left        -> "Blue"
                MiddleLeft  -> "Red"
                MiddleRight -> "Red"
                Right       -> "Blue"
                None        -> "white"

        drawNote : Lane -> Notes -> Int -> List (Svg Msg)
        drawNote lane notes time =
            case notes of
                Tap n ->
                    [ rect
                        [ x <| String.fromInt <| laneWidth lane
                        , y <| String.fromInt (time//20-n//model.bpm*10)
                        , width "50"
                        , height "5"
                        , fill <| notesColor lane
                        ]
                        []
                    ]

                Hold start end ->
                    [ rect
                        [ x <| String.fromInt <| laneWidth lane
                        , y <| String.fromInt (time//20-start//model.bpm*10)
                        , width "50"
                        , height "5"
                        , fill <| notesColor lane
                        ]
                        []
                    , rect
                        [ x <| String.fromInt <| laneWidth lane
                        , y <| String.fromInt (time//20-end//model.bpm*10)
                        , width "50"
                        , height "5"
                        , fill <| notesColor lane
                        ]
                        []
                    , rect
                        [ x <| String.fromInt <| (laneWidth lane) + 5
                        , y <| String.fromInt (time//20-end//model.bpm*10)
                        , width "40"
                        , height <| String.fromInt <| (end - start)//model.bpm*10
                        , fill <| notesColor lane
                        ]
                        []
                    ]

        drawNotes =
            [ Left, MiddleLeft, MiddleRight, Right ]
                |> List.concatMap (\lane -> model.score lane
                    |> List.map (\notes -> drawNote lane notes model.spentTime))
            |> List.concat

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
stringToLane str =
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

--Main--

main : Program () Model Msg
main =
    Browser.element
        { init = \_->(initialModel, perform Start Time.now )
        , view = view
        , update = update
        , subscriptions = subscription
        }