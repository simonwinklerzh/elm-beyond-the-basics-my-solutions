port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- model


type alias Customer =
    { id : String
    , name : String
    }


type alias Model =
    { name : String
    , customers : List Customer
    , error : Maybe String
    , editing : Maybe Customer
    }


initModel : Model
initModel =
    { name = ""
    , customers = []
    , error = Nothing
    , editing = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



-- update


type Msg
    = NameInput String
    | SaveCustomer
    | CustomerSaved String
    | CustomerAdded Customer
    | DeleteCustomer Customer
    | CustomerDeleted Customer
    | Edit Customer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameInput name ->
            ( { model | name = name }, Cmd.none )

        SaveCustomer ->
            ( model, addCustomer model.name )

        CustomerSaved key ->
            ( { model | name = "" }, Cmd.none )

        CustomerAdded customer ->
            let
                newCustomers =
                    customer :: model.customers
            in
            ( { model | customers = newCustomers }, Cmd.none )

        DeleteCustomer customer ->
            ( model, deleteCustomer customer )

        CustomerDeleted customer ->
            let
                newCustomers =
                    List.filter (\current -> current.id /= customer.id) model.customers
            in
            ( { model | customers = newCustomers }, Cmd.none )

        Edit customer ->
            ( { model | editing = Just customer }, Cmd.none )



-- view


viewCustomer : Maybe Customer -> Customer -> Html Msg
viewCustomer editingCustomer customer =
    let
        shouldDisableEdit =
            case editingCustomer of
                Just c ->
                    if c.id == customer.id then
                        False

                    else
                        True

                Nothing ->
                    False

        shouldEnableInput =
            case editingCustomer of
                Just c ->
                    if c.id == customer.id then
                        True

                    else
                        False

                Nothing ->
                    False
    in
    li []
        [ i
            [ class "remove"
            , onClick (DeleteCustomer customer)
            ]
            []
        , button
            [ disabled shouldDisableEdit
            , onClick (Edit customer)
            ]
            [ i
                [ class "edit" ]
                []
            ]
        , if shouldEnableInput then
            input
                [ value customer.name
                , type_ "text"
                ]
                []

          else
            text customer.name
        ]


viewCustomers : List Customer -> Maybe Customer -> Html Msg
viewCustomers customers editingCustomer =
    customers
        |> List.sortBy .id
        |> List.map (viewCustomer editingCustomer)
        |> ul []


viewCustomerForm : Model -> Html Msg
viewCustomerForm model =
    Html.form [ onSubmit SaveCustomer ]
        [ input [ type_ "text", onInput NameInput, value model.name ] []
        , text <| Maybe.withDefault "" model.error
        , button [ type_ "submit" ] [ text "Save" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Customer List" ]
        , viewCustomerForm model
        , viewCustomers model.customers model.editing
        ]


document : Model -> Browser.Document Msg
document model =
    Browser.Document "Hello World" [ view model ]



-- subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Sub.none
    Sub.batch
        [ customerSaved CustomerSaved
        , newCustomer CustomerAdded
        , customerDeleted CustomerDeleted
        ]



-- outgoing port


port addCustomer : String -> Cmd msg


port deleteCustomer : Customer -> Cmd msg



-- incoming ports


port customerSaved : (String -> msg) -> Sub msg


port newCustomer : (Customer -> msg) -> Sub msg


port customerDeleted : (Customer -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = document
        , subscriptions = subscriptions
        }
