port module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Json.Decode as D exposing (Decoder, field, string)

import Html exposing (Html, div, text, br, input)
import Html.Attributes exposing (id, class, type_, min, max, step, value)
import Html.Events exposing (onClick, onInput, targetValue)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill)
import Time exposing (Posix, every, posixToMillis)
import Task exposing (perform)

import ListWrapper.Dict as Dict exposing (Dict)
import ListWrapper.Set as Set exposing (Set)

port sound : String -> Cmd msg
port pause : String -> Cmd msg
port volumeChange : Float -> Cmd msg

type alias Model =
    { score        : Dict Lane (List Notes)
    , visibleNotes : Dict Lane (List Notes)
    , holdEvaluation : Dict Lane ( Evaluation, Notes )
    , elements     : List ( Int, Control )
    , spentTimeFromReferencePoint : Int
    , bpm       : Int
    , beatReferencePoint : ( Int, Int )
    , isPressed : Set Lane
    , speed : Float
    , grades : List Evaluation
    , phase : Phase
    , volume : Float
    , viewTestText : String
    , isPause : Bool
    }

initialModel : Model
initialModel =
    { score        = initialScore
    , elements     = initialElements
    , visibleNotes = initialScore
    , holdEvaluation = Dict.empty
    , spentTimeFromReferencePoint = 0        --[ms]
    , bpm       = 120      --[beats/min]
    , beatReferencePoint = ( 0, 0 )  --( [ms], [beatUnit] )
    , isPressed = Set.empty
    , speed = 1
    , grades = []
    , phase = Title
    , volume = 1
    , viewTestText = ""
    , isPause = False
    }

type Notes
    = Tap Int             --timing[beatUnit]
    | Hold Int Int        --startTiming[beatUnit] endTiming[beatUnit]

type Lane
    = Left
    | MiddleLeft
    | MiddleRight
    | Right

type Control
    = End
    | ChangeBPM Int  -- newBPM[beat/min]

beatUnit = 120 -- [ / beat]

initialScore =
    Dict.fromList
        [ ( Left, [ Hold 180 240, Tap 360, Tap 520, Tap 800 ] )
        , ( MiddleLeft, [ Tap 400, Tap 560, Tap 760 ] )
        , ( MiddleRight, [ Tap 440, Tap 600, Tap 720 ] )
        , ( Right, [ Tap 480, Tap 640, Tap 680, Tap 800 ] )
        ]

initialElements =
    [ ( 120, ChangeBPM 60 )
    , ( 480, ChangeBPM 120 )
    , ( 920, End )
    ]

type Msg
    = Pressed Lane
    | Released Lane
    | Tick Posix
    | Start Posix
    | None
    | KeyDownTest Option
    | SliderChanged Setting Float

type Setting
    = Volume
    | Speed

type Option
    = MoveToSettings
    | Resume
    | Escape
    | Else String

type Evaluation
    = CriticalPerfect         --100%(+1)
    | Perfect                 --100%
    | Great                   -- 75%
    | Good                    -- 50%
    | Miss                    --  0%

type Phase
    = Title
    | Settings
    | Play Bool
    | Result


--Update--

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SliderChanged set value ->
            let
                newModel =
                    case set of
                        Volume -> { model | volume = value }
                        Speed  -> { model | speed  = value }
            in
                ( newModel, Cmd.none )

        KeyDownTest opt ->
            case opt of
                MoveToSettings ->
                    case model.phase of
                        Title ->
                            ( { model | phase = Settings }, Cmd.none )
                        _ ->
                            ( model, Cmd.none )
                Escape ->
                    case model.phase of
                        Title ->
                            ( model, Cmd.none )
                        Settings ->
                            ( { model | phase = Title }, volumeChange model.volume )
                        Play bool->
                            let
                                cmd =
                                    if bool then sound "id" else pause "id"
                            in
                                ( { model | isPause = not model.isPause }, cmd )
                        Result ->
                            let
                                newModel =
                                    { model |
                                      visibleNotes = initialScore
                                    , holdEvaluation = Dict.empty
                                    , spentTimeFromReferencePoint = 0
                                    , bpm       = 120
                                    , beatReferencePoint = ( 0, 0 )
                                    , isPressed = Set.empty
                                    , grades = []
                                    , phase = Title
                                    }
                            in
                                ( newModel, Cmd.none )
                Resume ->
                    ( model, Cmd.none )
                Else str ->
                    case model.phase of
                        Title ->
                            ( { model | phase = Play False}, perform Start Time.now)
                        _ ->
                            ( { model | viewTestText = str }, Cmd.none )

        Pressed lane ->
            let
                newMsg =
                    case model.phase of
                        Title -> perform Start Time.now
                        _ -> Cmd.none
            in
                ( { model | phase = Play False}
                    |> setLaneState lane True
                    |> evaluate lane
                , newMsg
                )

        Released lane ->
            ( model
                |> setLaneState lane False
                |> holdEndEvaluate lane
            , Cmd.none
            )

        Tick posix ->
            let
                ( referenceTime, _ ) = model.beatReferencePoint
                newModel = { model | spentTimeFromReferencePoint = (posixToMillis posix) - referenceTime }

            in
                ( newModel
                    |> removeExpiredVisibleNotes
                    |> checkControls
                , Cmd.none
                )

        Start posix ->
            ( { model | beatReferencePoint = ( posixToMillis posix, 0 ) }
            , sound "id"
            )

        None ->
            ( model
            , Cmd.none
            )

checkControls : Model -> Model
checkControls model =
    let
        spentTimeInBeats = getSpentTimeInBeats model
    in
        case model.elements of
            [] ->
                model

            ( timing, control ) :: tl ->
                if spentTimeInBeats < timing
                then model
                else applyControl { model | elements = tl } control
                    |> checkControls

applyControl model ctrl =
    case ctrl of
        End -> { model | phase = Result }
        ChangeBPM newBPM ->
            let
                ( old, _ ) = model.beatReferencePoint
                newReferencePoint =
                    ( old + model.spentTimeFromReferencePoint, getSpentTimeInBeats model )
            in
                { model
                    | bpm = newBPM
                    , beatReferencePoint = newReferencePoint
                    , spentTimeFromReferencePoint = 0
                }

getSpentTimeInBeats model = model.spentTimeFromReferencePoint * model.bpm // 60 * beatUnit // 1000 + (Tuple.second model.beatReferencePoint)

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
        timeError beatUnits = abs (beatUnitToMillis (beatUnits - getSpentTimeInBeats model) )

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
                |> Maybe.andThen (\hold -> msToEvaluation model.spentTimeFromReferencePoint hold
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
        timeError beatUnits = abs (beatUnitToMillis (beatUnits - getSpentTimeInBeats model) )

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
                    |> List.concatMap (drawNote model spentTimeInBeats lane model.holdEvaluation))

        changeLaneColor =
            model.isPressed
                |> Set.toList
                |> List.concatMap drawPressedLane

        viewSvg =
             svg[](List.concat
                [ changeLaneColor
                , drawNotes
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
                            |> List.concatMap viewEvaluation
                    )
        viewTitle =
            div[]
                [ text "Press any key to start (Press 's' to open settings)"
                ]

        viewSettings =
            let
                speedString =
                    String.append (String.fromInt <| (//) 10 <| round <| model.speed * 10)
                        <| String.append "."
                        (String.fromInt <| modBy 10 <| round <| model.speed * 10)
            in
                div[]
                    [ text "Volume"
                    , input
                        [ type_ "range"
                        , min "0"
                        , max "1"
                        , step "0.01"
                        , value <| String.fromFloat <| model.volume
                        , onInput (String.toFloat >> Maybe.withDefault model.volume >> SliderChanged Volume)
                        ][]
                    , text <| String.fromInt <| round <| model.volume * 100
                    , br[][]
                    , text "Speed  "
                    , input
                        [ type_ "range"
                        , min "1"
                        , max "2"
                        , step "0.1"
                        , value <| String.fromFloat <| model.speed
                        , onInput (String.toFloat >> Maybe.withDefault model.speed >> SliderChanged Speed)
                        ][]
                    , text speedString
                    , br[][]
                    , text model.viewTestText
                    ]

    in
        case model.phase of
            Title ->
                div [][ viewTitle ]
            Settings ->
                div [][ viewSettings ]
            Play _->
                div [][ viewSvg ]
            Result ->
                div [][ viewGrades ]

pixelPerBeatUnit model = model.speed -- [px / beatUnit]
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

drawNote : Model -> Int -> Lane ->  Dict Lane ( Evaluation, Notes ) -> Notes -> List (Svg Msg) -- 現在の拍[beat unit], レーン, ノーツ
drawNote model now lane holdEvaluation note =
    case note of
        Tap at ->
            [ rect
                [ x <| String.fromInt <| lanePosition lane
                , y <| String.fromFloat <| (toFloat (judgeLine - (at - now))) * (pixelPerBeatUnit model)
                , width "50"
                , height "5"
                , fill <| laneColor lane
                ]
                []
            ]

        Hold start end ->
            let
                ( lengthOfHold, isDrawHoldStart ) =
                    case holdEvaluation |> Dict.get lane of
                        Nothing ->
                             ((toFloat (end - start)) * (pixelPerBeatUnit model), True )
                        Just ( _, holdingNotes ) ->
                            if holdingNotes == note
                            then ((toFloat (end - now)) * (pixelPerBeatUnit model), False )
                            else ((toFloat (end - start)) * (pixelPerBeatUnit model), True )
                drawHoldStart =
                    if isDrawHoldStart
                    then
                        [ rect
                            [ x <| String.fromInt <| lanePosition lane
                            , y <| String.fromFloat <| (toFloat (judgeLine - (start - now))) * (pixelPerBeatUnit model)
                            , width "50"
                            , height "5"
                            , fill <| laneColor lane
                            ]
                            []
                        ]
                    else
                        []


            in
                List.append drawHoldStart
                    [ rect
                        [ x <| String.fromInt <| lanePosition lane
                        , y <| String.fromFloat <| (toFloat (judgeLine - (end - now))) * (pixelPerBeatUnit model)
                        , width "50"
                        , height "5"
                        , fill <| laneColor lane
                        ]
                        []
                    , rect
                        [ x <| String.fromInt <| (lanePosition lane) + 5
                        ,y <| String.fromFloat <| (toFloat (judgeLine - (end - now))) * (pixelPerBeatUnit model)
                        , width "40"
                        , height <| String.fromInt <| round <| lengthOfHold
                        , fill <| laneColor lane
                        ]
                        []
                    ]

drawPressedLane : Lane -> List (Svg Msg)
drawPressedLane lane =
    [ rect
        [ x (lane |> lanePosition |> String.fromInt)
        , y "0"
        , width "50"
        , height (judgeLine |> String.fromInt)
        , fill "orange"
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
    case model.phase of
        Title ->
            onKeyDown keyDownTester
        Settings ->
            onKeyDown keyDownTester
        Play bool ->
            if bool
            then
                onKeyDown keyDownDecoder
            else
                Sub.batch
                    [ onKeyDown keyDownDecoder
                    , onKeyDown keyDownTester
                    , onKeyUp keyUpDecoder
                    , Time.every 16 Tick
                    ]
        Result ->
            onKeyDown keyDownTester

keyDownTester =
    (field "key" string)
        |> D.map stringToOptions
        |> D.map KeyDownTest

stringToOptions str =
    case str of
        "s" -> MoveToSettings
        "r" -> Resume
        "Escape" -> Escape
        _   -> Else str

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
        { init = \_-> (initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }