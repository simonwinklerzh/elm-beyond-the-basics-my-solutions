-- module Main exposing (Model)
port module Main exposing (main)

import Browser
import Debug
import Html exposing (Html, a, button, div, h1, input, p, span, text)
import Html.Attributes exposing (checked, disabled, href, size, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Cmd.Extra exposing (addCmd, addCmds, withCmd, withCmds, withNoCmd)
import Json.Encode exposing (Value)
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..), State)


handlers : List (Handler Model Msg)
handlers =
    [ WebSocketHandler socketHandler
    ]

funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict handlers getCmdPort

{-| Get a possibly simulated output port.
-}
getCmdPort : String -> Model -> (Value -> Cmd Msg)
getCmdPort moduleName model =
    PortFunnels.getCmdPort Process moduleName model.useSimulator


{-| The real output port.
-}
cmdPort : Value -> Cmd Msg
cmdPort =
    PortFunnels.getCmdPort Process "" False



-- Model


defaultUrl : String
defaultUrl =
    "wss://echo.websocket.org"


type alias Model =
    { stream_time : Bool
    , time : String

    -- From elm-websocket-client example
    , send : String
    , log : List String
    , url : String
    , useSimulator : Bool
    , wasLoaded : Bool
    , state : State
    , key : String
    , error : Maybe String
    }

test = Debug.log "number" 1 

initModel : Model
initModel =
    { stream_time = False
    , time = ""

    -- From elm-websocket-client example
    , send = "Hello World!"
    , log = []
    , url = defaultUrl
    , useSimulator = True
    , wasLoaded = False
    , state = PortFunnels.initialState
    , key = "socket"
    , error = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



-- Update


type Msg
    = UpdateSend String
    | UpdateUrl String
    | ToggleUseSimulator
    | ToggleAutoReopen
    | Connect
    | Close
    | Send
    | Process Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSend newsend ->
            { model | send = newsend } |> withNoCmd

        UpdateUrl url ->
            { model | url = url } |> withNoCmd

        ToggleUseSimulator ->
            { model | useSimulator = not model.useSimulator } |> withNoCmd

        ToggleAutoReopen ->
            let
                state =
                    model.state

                socketState =
                    state.websocket

                autoReopen =
                    WebSocket.willAutoReopen model.key socketState
            in
            { model
                | state =
                    { state
                        | websocket =
                            WebSocket.setAutoReopen
                                model.key
                                (not autoReopen)
                                socketState
                    }
            }
                |> withNoCmd

        Connect ->
            { model
                | log =
                    (if model.useSimulator then
                        "Connecting to simulator"

                     else
                        "Connecting to " ++ model.url
                    )
                        :: model.log
            }
                |> withCmd
                    (WebSocket.makeOpenWithKey model.key model.url
                        |> send model
                    )

        Send ->
            { model
                | log =
                    ("Sending \"" ++ model.send ++ "\"") :: model.log
            }
                |> withCmd
                    (WebSocket.makeSend model.key model.send
                        |> send model
                    )

        Close ->
            { model
                | log = "Closing" :: model.log
            }
                |> withCmd
                    (WebSocket.makeClose model.key
                        |> send model
                    )

        Process value ->
            case
                PortFunnels.processValue funnelDict value model.state model
            of
                Err error ->
                    { model | error = Just error } |> withNoCmd

                Ok res ->
                    res

send : Model -> WebSocket.Message -> Cmd Msg
send model message =
    WebSocket.send (getCmdPort WebSocket.moduleName model) message

doIsLoaded : Model -> Model
doIsLoaded model =
    if not model.wasLoaded && WebSocket.isLoaded model.state.websocket then
        { model
            | useSimulator = False
            , wasLoaded = True
        }

    else
        model


socketHandler : Response -> State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            doIsLoaded
                { mdl
                    | state = state
                    , error = Nothing
                }
    in
    case response of
        WebSocket.MessageReceivedResponse { message } ->
            { model | log = ("Received \"" ++ message ++ "\"") :: model.log }
                |> withNoCmd

        WebSocket.ConnectedResponse r ->
            { model | log = ("Connected: " ++ r.description) :: model.log }
                |> withNoCmd

        WebSocket.ClosedResponse { code, wasClean, expected } ->
            { model
                | log =
                    ("Closed, " ++ closedString code wasClean expected)
                        :: model.log
            }
                |> withNoCmd

        WebSocket.ErrorResponse error ->
            { model | log = WebSocket.errorToString error :: model.log }
                |> withNoCmd

        _ ->
            case WebSocket.reconnectedResponses response of
                [] ->
                    model |> withNoCmd

                [ ReconnectedResponse r ] ->
                    { model | log = ("Reconnected: " ++ r.description) :: model.log }
                        |> withNoCmd

                list ->
                    { model | log = Debug.toString list :: model.log }
                        |> withNoCmd


closedString : WebSocket.ClosedCode -> Bool -> Bool -> String
closedString code wasClean expected =
    "code: "
        ++ WebSocket.closedCodeToString code
        ++ ", "
        ++ (if wasClean then
                "clean"

            else
                "not clean"
           )
        ++ ", "
        ++ (if expected then
                "expected"

            else
                "NOT expected"
           )



-- View


b : String -> Html Msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


docp : String -> Html Msg
docp string =
    p [] [ text string ]


view : Model -> Html Msg
view model =
    let
        isConnected =
            WebSocket.isConnected model.key model.state.websocket
    in
    div
        [ style "width" "40em"
        , style "margin" "auto"
        , style "margin-top" "1em"
        , style "padding" "1em"
        , style "border" "solid"
        ]
        [ h1 [] [ text "PortFunnel.WebSocket Example" ]
        , p []
            [ input
                [ value model.send
                , onInput UpdateSend
                , size 50
                ]
                []
            , text " "
            , button
                [ onClick Send
                , disabled (not isConnected)
                ]
                [ text "Send" ]
            ]
        , p []
            [ b "url: "
            , input
                [ value model.url
                , onInput UpdateUrl
                , size 30
                , disabled isConnected
                ]
                []
            , text " "
            , if isConnected then
                button [ onClick Close ]
                    [ text "Close" ]

              else
                button [ onClick Connect ]
                    [ text "Connect" ]
            , br
            , b "use simulator: "
            , input
                [ type_ "checkbox"
                , onClick ToggleUseSimulator
                , checked model.useSimulator
                , disabled isConnected
                ]
                []
            , br
            , b "auto reopen: "
            , input
                [ type_ "checkbox"
                , onClick ToggleAutoReopen
                , checked <|
                    WebSocket.willAutoReopen
                        model.key
                        model.state.websocket
                ]
                []
            ]
        , p [] <|
            List.concat
                [ [ b "Log:"
                  , br
                  ]
                , List.intersperse br (List.map text model.log)
                ]
        , div []
            [ b "Instructions:"
            , docp <|
                "Fill in the 'url' and click 'Connect' to connect to a real server."
                    ++ " This will only work if you've connected the port JavaScript code."
            , docp "Fill in the text and click 'Send' to send a message."
            , docp "Click 'Close' to close the connection."
            , docp <|
                "If the 'use simulator' checkbox is checked at startup,"
                    ++ " then you're either runing from 'elm reactor' or"
                    ++ " the JavaScript code got an error starting."
            , docp <|
                "Uncheck the 'auto reopen' checkbox to report when the"
                    ++ " connection is lost unexpectedly, rather than the deault"
                    ++ " of attempting to reconnect."
            ]
        , p []
            [ b "Package: "
            , a [ href "https://package.elm-lang.org/packages/billstclair/elm-websocket-client/latest" ]
                [ text "billstclair/elm-websocket-client" ]
            , br
            , b "GitHub: "
            , a [ href "https://github.com/billstclair/elm-websocket-client" ]
                [ text "github.com/billstclair/elm-websocket-client" ]
            ]
        ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions =
    PortFunnels.subscriptions Process


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
