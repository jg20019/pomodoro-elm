port module Main exposing (..)

import Browser
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (classList, style)
import Html.Events exposing (on, preventDefaultOn, onClick, onMouseUp, onMouseOut)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- PORTS


port playAlarm : String -> Cmd msg


port stopAlarm : String -> Cmd msg


port receiveTick : (String -> msg) -> Sub msg



-- MODEL


type alias Seconds =
    Int


type Timer
    = Initial Seconds
    | Running Seconds
    | Paused Seconds

type alias Pos = 
    { x : Int
    , y : Int
    }

type alias Model =
    { timer : Timer
    , completed : Int
    , alarmRunning : Bool
    , top: Int
    , left: Int
    , drag: Maybe Pos
    }


init_minutes : Int
init_minutes =
    25


seconds_per_minute : Int
seconds_per_minute =
    60


initialTimer : Timer
initialTimer =
    Initial (init_minutes * seconds_per_minute)


secondsFromTimer : Timer -> Int
secondsFromTimer timer =
    case timer of
        Initial s ->
            s

        Running s ->
            s

        Paused s ->
            s


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model initialTimer 0 False 20 20 Nothing, Cmd.none )



-- UPDATE


type Msg
    = Tick String
    | Start
    | Continue
    | Stop
    | ResetTimer
    | MouseDown Int Int
    | MouseMove Int Int
    | MouseUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( startTimer model, Cmd.none )

        Continue ->
            ( continueTimer model, Cmd.none )

        Stop ->
            ( stopTimer model, Cmd.none )

        Tick _ ->
            tick model

        ResetTimer ->
            resetTimer model

        MouseMove x y -> 
            case model.drag of
                Nothing -> 
                    (model, Cmd.none)
                Just pos -> 
                    (drag model pos x y, Cmd.none)

        MouseDown x y -> 
            (startDrag model x y, Cmd.none)

        MouseUp -> 
            (stopDrag model, Cmd.none)

startTimer : Model -> Model
startTimer model =
    { model | timer = Running (secondsFromTimer model.timer) }


stopTimer : Model -> Model
stopTimer model =
    case model.timer of
        Running s ->
            { model | timer = Paused s }

        _ ->
            model


continueTimer : Model -> Model
continueTimer model =
    case model.timer of
        Paused s ->
            { model | timer = Running s }

        _ ->
            model


resetTimer : Model -> ( Model, Cmd Msg )
resetTimer model =
    ( { model | timer = initialTimer, alarmRunning = False }, stopAlarm "" )


tick : Model -> ( Model, Cmd Msg )
tick model =
    case model.timer of
        Running s ->
            let
                remaining_seconds =
                    max 0 (s - 1)
            in
            if remaining_seconds == 0 then
                ( { model | timer = initialTimer, completed = model.completed + 1, alarmRunning = True }, playAlarm "" )

            else
                ( { model | timer = Running remaining_seconds }, Cmd.none )

        _ ->
            ( model, Cmd.none )

startDrag : Model -> Int -> Int -> Model 
startDrag model x y = 
    let 
        pos = Pos x y
    in 
    { model | drag = Just pos } 

drag : Model -> Pos -> Int -> Int -> Model 
drag model dragStart x y = 
    let 
       dx = dragStart.x - x
       dy = dragStart.y - y
       left = model.left - dx
       top = model.top - dy 
       pos = Just (Pos left top)
    in 
    { model | left = left, top = top, drag = pos } 

stopDrag : Model -> Model
stopDrag model  = 
    { model | drag = Nothing } 

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.timer of
        Running _ ->
            receiveTick Tick

        _ ->
            Sub.none



-- VIEW

view : Model -> Html Msg
view model = 
    div [ style "top" "0px"
        , style "left" "0px"
        , style "position" "relative" 
        , style "width" "100%"
        , style "height" "100%"
        , style "background-color" "grey"
        , onMouseMove MouseMove 
        , onMouseUp MouseUp
        ]
        [ viewTimerWidget model
        ]

viewTimerWidget : Model -> Html Msg
viewTimerWidget model =
    div [ classList [ ("timer", True) ]
        , style "top" (px model.top)
        , style "left" (px model.left)
        , style "position" "absolute"
        , onMouseDown MouseDown
        , onMouseUp MouseUp
        ]
        [ viewCompleted model.completed
        , viewTimer model.timer
        , viewControls model
        ]

viewCompleted : Int -> Html Msg
viewCompleted completed = 
    span [] [ text ("Completed: " ++ String.fromInt completed ) ]

viewTimer : Timer -> Html Msg
viewTimer timer =
    let
        s =
            secondsFromTimer timer
    in
    h1
        [ classList
            [ ( "green", s > 300 )
            , ( "red", s <= 300 )
            ]
        ]
        [ text (timerStr timer)
        ]


timerStr : Timer -> String
timerStr timer =
    secondsFromTimer timer
        |> secondsToTimerStr


secondsToTimerStr : Int -> String
secondsToTimerStr n =
    let
        m =
            zeroPad (minutes n)

        s =
            zeroPad (seconds n)
    in
    m ++ ":" ++ s



-- minutes: returns minutes from seconds


minutes : Int -> Int
minutes s =
    s // seconds_per_minute



-- seconds: returns seconds remaing in timer


seconds : Int -> Int
seconds s =
    modBy 60 s



-- zeroPad: add leading zero to single digit string


zeroPad : Int -> String
zeroPad n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n


viewControls : Model -> Html Msg
viewControls model =
    div
        [ classList
            [ ( "controls", True )
            ]
        ]
        [ mainButton model
        , resetButton
        ]


mainButton : Model -> Html Msg
mainButton model =
    case model.timer of
        Initial _ ->
            if model.alarmRunning then
                okButton 
            else
                startButton Start

        Running _ ->
            stopButton

        Paused _ ->
            startButton Continue


startButton : Msg -> Html Msg
startButton msg =
    button [ onClick msg ] [ text "Start" ]

-- button to stop the alarm
okButton : Html Msg 
okButton = 
    button [ onClick ResetTimer ] [ text "Ok" ] 

stopButton : Html Msg
stopButton =
    button [ onClick Stop ] [ text "Stop" ]

resetButton : Html Msg
resetButton =
    button [ onClick ResetTimer ] [ text "Reset" ]

-- Attributes Helpers
px : Int -> String
px pixels = 
    (String.fromInt pixels) ++ "px"

-- Events 
posDecoder : (Int -> Int -> msg) -> Decode.Decoder msg
posDecoder message = 
    Decode.map2 message
       (Decode.field "x" Decode.int)
       (Decode.field "y" Decode.int)

onMouseMove : (Int -> Int -> msg) -> Attribute msg
onMouseMove message = 
    on "mousemove" (posDecoder message)

onMouseDown : (Int -> Int -> msg) -> Attribute msg
onMouseDown message = 
    preventDefaultOn "mousedown" (Decode.map alwaysPreventDefault (posDecoder message))

alwaysPreventDefault : msg -> (msg, Bool)
alwaysPreventDefault msg = 
    (msg, True)
