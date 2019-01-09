module Main exposing (Model, Msg(..), initModel, main, update, view)

import Browser
import Html
import Html.Events exposing (onClick)
import Http



-- model


type alias Model =
    String


initModel : Model
initModel =
    "Finding a joke..."


getRandomJoke : Cmd Msg
getRandomJoke =
    Http.get
        { url = "https://api.icndb.com/jokes/random"
        , expect = Http.expectString GotText
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, getRandomJoke )



-- update


type Msg
    = GotText (Result Http.Error String)
    | FetchNewQuote


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText (Ok joke) ->
            ( joke, Cmd.none )

        GotText (Err err) ->
            ( model, Cmd.none )

        FetchNewQuote ->
            ( model, getRandomJoke )



-- view


view : Model -> Html.Html Msg
view model =
    Html.div
        []
        [ Html.p
            []
            [ Html.text "Chuck Norris Random Quote" ]
        , Html.blockquote
            []
            [ Html.text model ]
        , Html.button
            [ onClick FetchNewQuote ]
            [ Html.text "Fetch new" ]
        ]



-- subscription


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
