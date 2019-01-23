module Main exposing (Model)

import Browser
import Html
import Html.Events exposing (onClick)



-- Model


type alias Model =
    { stream_time : Bool
    , time : String
    }


initModel : Model
initModel =
    { stream_time = False
    , time = ""
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



-- Update


type Msg
    = Toggle_streaming


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle_streaming ->
            ( { model | stream_time = not model.stream_time }, Cmd.none )



-- View


view : Model -> Html.Html Msg
view model =
    let
        toggle_label =
            if model.stream_time then
                "Stop"

            else
                "Start"
    in
    Html.div
        []
        [ Html.button
            [ onClick Toggle_streaming ]
            [ Html.text toggle_label ]
        , Html.br
            []
            []
        , Html.text model.time
        ]



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
