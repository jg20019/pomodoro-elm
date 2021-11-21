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


type alias Seconds =
    Int


type Timer
    = Initial Seconds
    | Running Seconds
    | Paused Seconds
    | Stopped -- a timer that was stopped prematurely
    | Finished -- a timer that ran until time ran out


type alias Model =
    { timer : Timer
    , completed : Int
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

        Stopped ->
            0

        Finished ->
            0


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model initialTimer 0, Cmd.none )



-- UPDATE


type Msg
    = Tick String
    | Start
    | Pause
    | Continue
    | Stop
    | ResetTimer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( startTimer model, Cmd.none )

        Pause ->
            ( pauseTimer model, Cmd.none )

        Continue ->
            ( continueTimer model, Cmd.none )

        Stop ->
            ( stopTimer model, Cmd.none )

        Tick _ ->
            tick model

        ResetTimer ->
            resetTimer model


startTimer : Model -> Model
startTimer model =
    { model | timer = Running (secondsFromTimer model.timer) }


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


resetTimer : Model -> ( Model, Cmd Msg )
resetTimer model =
    ( { model | timer = initialTimer, completed = model.completed + 1 }, stopAlarm "" )


tick : Model -> ( Model, Cmd Msg )
tick model =
    case model.timer of
        Running s ->
            let
                remaining_seconds =
                    max 0 (s - 1)
            in
            if remaining_seconds == 0 then
                ( { model | timer = Finished }, playAlarm "" )

            else
                ( { model | timer = Running remaining_seconds }, Cmd.none )

        _ ->
            ( model, Cmd.none )



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
    div [ classList [ ( "timer", True ) ] ]
        [ viewTimerCard model
        ]


viewTimerCard : Model -> Html Msg
viewTimerCard model =
    case model.timer of
        Finished ->
            viewFinishedTimer model.completed

        Initial _ ->
            viewInitialTimer model.timer

        _ ->
            viewRunningTimer model.timer


viewInitialTimer : Timer -> Html Msg
viewInitialTimer timer =
    div []
        [ viewTimer timer
        , viewInitialControls
        ]


viewInitialControls : Html Msg
viewInitialControls =
    div [ classList [ ( "controls", True ) ] ]
        [ startButton Start ]


viewRunningTimer : Timer -> Html Msg
viewRunningTimer timer =
    div []
        [ viewTimer timer
        , viewControls timer
        ]


viewFinishedTimer : Int -> Html Msg
viewFinishedTimer completed =
    let
        c =
            String.fromInt <| completed + 1

        msg =
            if completed + 1 == 1 then
                "You have completed 1 pomodoro."

            else
                "You have completed " ++ c ++ " pomodoros"
    in
    div []
        [ h1 [] [ text msg ]
        , div [ classList [ ( "controls", True ) ] ]
            [ button [ onClick ResetTimer ] [ text "Reset" ] ]
        ]


viewTimer : Timer -> Html Msg
viewTimer timer =
    let
        s =
            secondsFromTimer timer
    in
    div
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


viewControls : Timer -> Html Msg
viewControls timer =
    div
        [ classList
            [ ( "controls", True )
            ]
        ]
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
