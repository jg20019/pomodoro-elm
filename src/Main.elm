port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)

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


type alias Seconds = Int


type Timer 
    = Initial Seconds
    | Running Seconds
    | Paused Seconds
    | Stopped -- a timer that was stopped prematurely
    | Finished -- a timer that ran until time ran out


type alias Model 
    = { timer: Timer
      , completed: Int
      }


init_minutes: Int
init_minutes = 
    25

seconds_per_minute : Int
seconds_per_minute = 
    60


initialTimer : Timer
initialTimer = 
    Initial (init_minutes * seconds_per_minute) 


init: () -> (Model, Cmd Msg)
init _ = 
    (Model initialTimer 0, Cmd.none)


-- UPDATE

type Msg 
    = Tick String
    | Start
    | Pause
    | Continue
    | Stop 
    | ResetTimer

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Start ->
            (startTimer model, Cmd.none)
        Pause ->
            (pauseTimer model, Cmd.none)
        Continue ->
            (continueTimer model, Cmd.none)
        Stop -> 
            (stopTimer model, Cmd.none)
        Tick _ -> 
            tick model
        ResetTimer ->
            resetTimer model


startTimer : Model -> Model
startTimer model = 
    let 
        s = init_minutes * seconds_per_minute 
    in
    { model | timer = Running s }


pauseTimer : Model -> Model
pauseTimer model = 
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


stopTimer : Model -> Model
stopTimer model = 
    { model | timer = Stopped } 
  
resetTimer : Model -> (Model, Cmd Msg)
resetTimer model =
    ({ model | timer = initialTimer }, stopAlarm "")

tick : Model -> (Model, Cmd Msg)
tick model = 
    case model.timer of
        Running s -> 
            let
                remaining_seconds = max 0 (s - 1) 
            in
                if remaining_seconds == 0 then
                    ({ model | timer = Finished }, playAlarm "")
                else 
                    ({ model | timer = Running remaining_seconds }, Cmd.none)
        _ ->
           ( model, Cmd.none)


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
    div [ classList [("timer", True)] ]
        [ viewTimerCard model.timer 
        ] 


viewTimerCard: Timer -> Html Msg
viewTimerCard timer = 
    case timer of
        Finished -> 
            viewFinishedTimer
        Initial _ ->
            viewInitialTimer timer
        _ ->
            viewRegularTimer timer

viewInitialTimer : Timer -> Html Msg
viewInitialTimer timer = 
    div []
        [ viewTimer timer
        , viewInitialControls 
        ]

viewInitialControls : Html Msg
viewInitialControls = 
    div [ classList [("controls", True)] ]
        [ startButton Start ]
            
            
viewFinishedTimer :  Html Msg 
viewFinishedTimer = 
    div []
        [ h1 [] [ text "Finished!" ]
        , button [ onClick ResetTimer ] [ text "Reset" ] 
        ] 

viewRegularTimer : Timer -> Html Msg
viewRegularTimer timer = 
    div []
        [ viewTimer timer
        , viewControls timer
        ]

viewTimer : Timer -> Html Msg
viewTimer timer = 
    div [] 
        [ text (timerStr timer)
        ]



timerStr : Timer -> String
timerStr timer = 
    case timer of
        Initial s -> 
            secondsToTimerStr s
        Running s -> 
            secondsToTimerStr s
        Paused s ->
            secondsToTimerStr s
        Stopped ->
            "00:00"
        Finished -> 
            "00:00"

secondsToTimerStr : Int -> String
secondsToTimerStr n = 
    let
        m = zeroPad (minutes n)
        s = zeroPad (seconds n)
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

viewControls : Timer -> Html Msg
viewControls timer = 
    div [] 
        [ toggleButton timer 
        , stopButton
        ]

toggleButton : Timer -> Html Msg
toggleButton timer = 
    case timer of
        Initial _ ->
            startButton Start
        Stopped -> 
            startButton Start
        Running _ ->
            pauseButton
        Paused _ ->
            startButton Continue
        Finished -> 
            restartButton 

startButton : Msg -> Html Msg
startButton msg = 
    button [ onClick msg ] [ text "Start" ]

restartButton : Html Msg
restartButton = 
    button [ onClick Start ] [ text "Restart" ] 

pauseButton : Html Msg
pauseButton = 
    button [ onClick Pause ] [ text "Pause" ] 


stopButton : Html Msg
stopButton = 
    button [ onClick Stop ] [ text "Stop" ] 
