module Main exposing (Model, Msg(..), initModel, main, update, view)

import Browser
import Html
import Html.Events exposing (onClick)
import Http
import Json.Decode



-- decoder


type alias Response =
    { id : Int
    , joke : String
    , categories : List String
    }


fullJokeDecoder : Json.Decode.Decoder Response
fullJokeDecoder =
    Json.Decode.map3 Response
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "joke" Json.Decode.string)
        (Json.Decode.field "categories" (Json.Decode.list Json.Decode.string))
        |> Json.Decode.at [ "value" ]



-- model


type alias Model =
    { text : String
    , categories : List String
    , error : Maybe String
    }


initModel : Model
initModel =
    { text = "Finding a joke..."
    , categories = []
    , error = Nothing
    }


getRandomJoke : Cmd Msg
getRandomJoke =
    Http.get
        { url = "https://api.icndb.com/jokes/random"
        , expect = Http.expectJson GotJoke fullJokeDecoder
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, getRandomJoke )



-- update


type Msg
    = GotJoke (Result Http.Error Response)
    | FetchNewQuote


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotJoke (Ok response) ->
            ( { model
                | error = Nothing
                , text = response.joke
                , categories = response.categories
              }
            , Cmd.none
            )

        GotJoke (Err err) ->
            case err of
                Http.BadUrl url ->
                    ( { model | error = Just ("Bad url: " ++ url) }, Cmd.none )

                Http.Timeout ->
                    ( { model | error = Just "Timeout. " }, Cmd.none )

                Http.NetworkError ->
                    ( { model | error = Just "Network Error " }, Cmd.none )

                Http.BadStatus statusNumber ->
                    ( { model | error = Just ("Bad Status " ++ String.fromInt statusNumber) }, Cmd.none )

                Http.BadBody body ->
                    ( { model | error = Just ("Bad Body: " ++ body) }, Cmd.none )

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
            [ Html.text
                (case model.error of
                    Nothing ->
                        model.text

                    Just error ->
                        Maybe.withDefault "" model.error
                )
            ]
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
