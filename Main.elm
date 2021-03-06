module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Json.Decode as D exposing (Decoder, field, string)

import Html exposing (Html, div, text, br)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Svg exposing (Svg, svg, rect, use)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill, xlinkHref)
import Time exposing (Posix, every, posixToMillis)
import Task exposing (perform)

import ListWrapper.Dict as Dict exposing (Dict)
import ListWrapper.Set as Set exposing (Set)

type alias Model =
    { score        : Dict Lane (List Notes)
    , visibleNotes : Dict Lane (List Notes)
    , holdEvaluation : Dict Lane ( Evaluation, Notes )
    , elements     : List Control
    , startTime : Int
    , spentTime : Int
    , bpm       : Int
    , isPressed : Set Lane
    , speed : Int
    , grades : List Evaluation
    , phase : Phase
    }

initialModel : Model
initialModel =
    { score        = initialScore
    , elements     = initialElements
    , visibleNotes = initialScore
    , holdEvaluation = Dict.empty
    , startTime = 0        --[ms]
    , spentTime = 0        --[ms]
    , bpm       = 60      --[beats/min]
    , isPressed = Set.empty
    , speed = 0
    , grades = []
    , phase = Play
    }

type Notes
    = Tap Int             --[beatUnit]
    | Hold Int Int        --[beatUnit]

type Lane
    = Left
    | MiddleLeft
    | MiddleRight
    | Right

type Control
    = End Int             --[beatUnit]
    | ChangeBPM Int Int   --[beat/min] [beatUnit]

beatUnit = 120 -- [ / beat]

initialScore =
    Dict.fromList
        [ ( Left, [ Hold 180 240, Tap 360, Tap 520, Tap 800 ] )
        , ( MiddleLeft, [ Tap 400, Tap 560, Tap 760 ] )
        , ( MiddleRight, [ Tap 440, Tap 600, Tap 720 ] )
        , ( Right, [ Tap 480, Tap 640, Tap 680, Tap 800 ] )
        ]

initialElements =
    (End 920)::
    [ ChangeBPM 120 0
    , ChangeBPM 60 240
    ]

type Msg
    = Pressed Lane
    | Released Lane
    | Tick Posix
    | Start Posix
    | None

type Evaluation
    = CriticalPerfect         --100%(+1)
    | Perfect                 --100%
    | Great                   -- 75%
    | Good                    -- 50%
    | Miss                    --  0%

type Phase
    = Play
    | Result


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
                |> holdEndEvaluate lane
            , Cmd.none
            )

        Tick posix ->
            let
                newModel = { model | spentTime = (posixToMillis posix) - model.startTime }
            in
                ( newModel
                    |> removeExpiredVisibleNotes
                    |> changePhase
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

changePhase : Model -> Model
changePhase model =
    let
        spentTimeInBeats = getSpentTimeInBeats model
        chooseEndElement control =
            case control of
                End _ -> True
                _ -> False

        endTime =
            model.elements
                |> List.filter chooseEndElement
                |> List.head

        newPhase =
            case endTime of
                Just (End endBeatUnit) ->
                    if endBeatUnit < spentTimeInBeats
                        then Result
                        else Play
                _ -> Play

    in
        { model | phase = newPhase }

getSpentTimeInBeats model = model.spentTime * model.bpm // 60 * beatUnit // 1000

removeExpiredVisibleNotes : Model -> Model
removeExpiredVisibleNotes model =
    let
        spentTimeInBeats = getSpentTimeInBeats model

        notesTimeCheck : Notes -> Bool
        notesTimeCheck notes=
            case notes of
                Tap time->
                    ( spentTimeInBeats - time > 120 )
                Hold start end->
                    ( spentTimeInBeats - end > 120 )

        listNotesUpdater oldNotes = listNotesUpdaterHelp oldNotes []
        listNotesUpdaterHelp : List Notes -> List Notes -> List Notes
        listNotesUpdaterHelp newNotes oldNotes =
            case oldNotes of
                [] -> newNotes
                hd :: tl ->
                    listNotesUpdaterHelp
                        (if (notesTimeCheck hd) then (hd :: newNotes) else newNotes) tl

        newVisibleNotes =
            model.visibleNotes
                |> Dict.toList
                |> List.map (\( lane, notes ) -> ( lane, listNotesUpdater notes ))
                |> Dict.fromList
    in
        { model | visibleNotes = newVisibleNotes }


evaluate : Lane -> Model -> Model
evaluate lane model =
    let
        beatUnitToMillis beatUnits = beatUnits * 1000 * 60 // model.bpm // beatUnit
        timeError beatUnits = abs (beatUnitToMillis beatUnits - model.spentTime)

        msToEvaluation : Int -> Notes -> Maybe Evaluation
        msToEvaluation pressedTime notes =
            case notes of
                Tap notesTime ->
                    if (timeError notesTime<=33) then Just CriticalPerfect
                    else if (timeError notesTime<=50) then Just Perfect
                    else if (timeError notesTime<=100) then Just Great
                    else if (timeError notesTime<=200) then Just Good
                    else if (timeError notesTime<=400) then Just Miss
                    else Nothing
                Hold start end ->
                    if (timeError start<=33) then Just CriticalPerfect
                    else if (timeError start<=50) then Just Perfect
                    else if (timeError start<=100) then Just Great
                    else if (timeError start<=200) then Just Good
                    else if (timeError start<=400) then Just Miss
                    else Nothing

        noteToError note =
            case note of
                Tap n -> timeError n
                Hold start end -> timeError start

        sortedNotes =
            model.visibleNotes
                |> Dict.get lane
                |> Maybe.withDefault []
                |> List.sortBy noteToError

        decideNoteForEvaluating = List.head sortedNotes

        newVisibleNotes =
            case decideNoteForEvaluating of
                Just (Hold _ _) ->
                    model.visibleNotes
                        |> Dict.get lane
                        |> Maybe.withDefault []

                _ -> List.drop 1 sortedNotes

        holdForEvaluating =
            decideNoteForEvaluating
                |> Maybe.andThen (\hold -> msToEvaluation model.spentTime hold
                    |> Maybe.map (\grade -> ( grade, hold )))

    in
        case holdForEvaluating of
            Nothing -> model
            Just ( grade, hold ) -> { model
                | grades = grade :: model.grades
                , visibleNotes =
                    model.visibleNotes
                        |> Dict.insert lane newVisibleNotes
                , holdEvaluation =
                    model.holdEvaluation
                        |> Dict.insert lane ( grade, hold )
                }

holdEndEvaluate : Lane -> Model -> Model
holdEndEvaluate lane model =
    let
        beatUnitToMillis beatUnits = beatUnits * 1000 * 60 // model.bpm // beatUnit
        timeError beatUnits = abs (beatUnitToMillis beatUnits - model.spentTime)

        msToEvaluation : ( Evaluation, Notes ) -> Maybe Evaluation
        msToEvaluation holdNotes =
            case holdNotes of
                ( evaluation, Hold start end ) ->
                    if (timeError end <= 200)
                    then
                        Just evaluation
                    else
                        if (timeError end <= 400)
                        then Just Miss
                        else Nothing
                _ -> Nothing

        evaluateResult =
            model.holdEvaluation
                |> Dict.get lane
                |> Maybe.andThen msToEvaluation

        visibleNotesUpdater value =
            case ( value, Dict.get lane model.holdEvaluation) of
                ( Just list, Just ( _, hold ) )  ->
                    list
                        |> List.filter ((/=) hold)
                        |> Just

                ( old, _ ) ->
                    old
    in
        { model |
          visibleNotes =
            model.visibleNotes
                |> Dict.update lane visibleNotesUpdater
        , holdEvaluation =
            model.holdEvaluation
                |> Dict.remove lane
        , grades =
            case evaluateResult of
                Nothing -> model.grades
                Just ev -> ev :: model.grades
        }


setLaneState: Lane -> Bool -> Model -> Model
setLaneState lane bool model =
    { model | isPressed = model.isPressed
        |> if bool then Set.insert lane else Set.remove lane }


--View--

view : Model -> Html Msg
view model =
    let
        spentTimeInBeats = getSpentTimeInBeats model

        drawNotes =
            model.visibleNotes
                |> Dict.toList
                |> List.concatMap (\( lane, notes ) -> notes
                    |> List.concatMap (drawNote spentTimeInBeats lane))

        changeLaneColor =
            model.isPressed
                |> Set.toList
                |> List.concatMap drawPressedLane

        rackOfHold =
            model.holdEvaluation
                |> Dict.keys
                |> List.concatMap drawRackOfHold

        viewSvg =
             svg[](List.concat
                [ changeLaneColor
                , drawNotes
                , rackOfHold
                , drawJudgeLine
                ])

        viewGrades : Html Msg
        viewGrades  =
            let
                evaruationToString ev =
                    case ev of
                        CriticalPerfect -> "CriticalPerfect"
                        Perfect         -> "Perfect"
                        Great           -> "Great"
                        Good            -> "Good"
                        Miss            -> "Miss"

                viewEvaluation ev =
                    [ text <| (evaruationToString ev) ++ " : "
                    , model.grades
                        |> List.filter ((==) ev)
                        |> List.length
                        |> String.fromInt
                        |> text
                    , br[][]
                    ]
            in
                div[]
                    <| List.append
                    [ text "Result"
                    , br[][]
                    ]
                    (
                        [CriticalPerfect, Perfect, Great, Good, Miss]
                            |> List.map viewEvaluation
                            |> List.concat
                    )

    in
        case model.phase of
            Play ->
                div [][ viewSvg, br[][] ]
            Result ->
                div [][ viewGrades ]

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
        Left        -> "url(#radical-blue)"
        MiddleLeft  -> "url(#radical-red)"
        MiddleRight -> "url(#radical-red)"
        Right       -> "url(#radical-blue)"

drawNote : Int -> Lane -> Notes -> List (Svg Msg) -- 現在の拍[beat unit], レーン, ノーツ
drawNote now lane note =
    case note of
        Tap at ->
            [ use
                [ xlinkHref "#tap-red"
                , x <| String.fromInt <| lanePosition lane
                , y <| String.fromInt (judgeLine - ((at - now) * pixelPerBeatUnit))
                , fill <| laneColor lane
                ]
                []
            ]

        Hold start end ->
            [ rect
                [ x <| String.fromInt <| (lanePosition lane) + 5
                , y <| String.fromInt <| (judgeLine - ((end - now) * pixelPerBeatUnit))
                , width "40"
                , height <| String.fromInt <| (end - start) * pixelPerBeatUnit
                , fill <| laneColor lane
                ]
                []
            , rect
                [ x <| String.fromInt <| lanePosition lane
                , y <| String.fromInt <| (judgeLine - ((start - now) * pixelPerBeatUnit))
                , width "50"
                , height "5"
                , fill <| laneColor lane
                ]
                []
            , rect
                [ x <| String.fromInt <| lanePosition lane
                , y <| String.fromInt <| (judgeLine - ((end - now) * pixelPerBeatUnit))
                , width "50"
                , height "5"
                , fill <| laneColor lane
                ]
                []
            ]

drawRackOfHold : Lane -> List (Svg Msg)
drawRackOfHold lane =
    [ rect
        [ x (lane |> lanePosition |> String.fromInt)
        , y (judgeLine + 2 |> String.fromInt)
        , width "50"
        , height "35"
        , fill "white"
        ][]
    ]

drawPressedLane : Lane -> List (Svg Msg)
drawPressedLane lane =
    [ rect
        [ x (lane |> lanePosition |> String.fromInt)
        , y "0"
        , width "50"
        , height (judgeLine |> String.fromInt)
        , fill "url(#lane-light)"
        ]
        []
    ]

drawJudgeLine =
    [ rect
        [ x "0"
        , y (judgeLine |> String.fromInt)
        , width "200"
        , height "2"
        , fill "black"
        ]
        []
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown keyDownDecoder
        , onKeyUp keyUpDecoder
        , Time.every 16 Tick
        ]

keyDownDecoder =
    (field "key" string)
        |> D.map stringToLane
        |> D.map (Maybe.map Pressed >> Maybe.withDefault None)

keyUpDecoder =
    (field "key" string)
        |> D.map stringToLane
        |> D.map (Maybe.map Released >> Maybe.withDefault None)

stringToLane : String -> Maybe Lane
stringToLane str =
    case str of
        "f" -> Just Left
        "g" -> Just MiddleLeft
        "h" -> Just MiddleRight
        "j" -> Just Right
        _   -> Nothing



--Main--

main : Program () Model Msg
main =
    Browser.element
        { init = \_->(initialModel, perform Start Time.now )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }