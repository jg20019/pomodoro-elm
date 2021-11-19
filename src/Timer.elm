port module Timer exposing (..)

import Time
import Platform exposing (..)

main : Program () Model Msg
main = 
    Platform.worker 
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

-- PORTS

port sendTick : String -> Cmd msg
port pauseTimer : (String -> msg) -> Sub msg
port startTimer : (String -> msg) -> Sub msg

type Timer 
    = Running
    | Paused

type alias Model = Timer

init : () -> (Model, Cmd msg)
init _ = 
    (Running, Cmd.none)

type Msg
    = Start
    | Pause
    | Tick Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Start -> 
            (Running, Cmd.none)
        Pause -> 
            (Paused, Cmd.none)
        Tick _ -> 
            (model, sendTick "")

subscriptions : Model -> Sub Msg
subscriptions model = 
    case model of
        Running -> 
            Time.every 1000 Tick

        Paused ->
            Sub.none
