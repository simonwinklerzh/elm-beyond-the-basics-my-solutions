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


type alias CustomerId =
    String


type alias Model =
    { name : String
    , id : Maybe CustomerId
    , customers : List Customer
    , error : Maybe String
    }


initModel : Model
initModel =
    { name = ""
    , id = Nothing
    , customers = []
    , error = Nothing
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
    | CustomerChanged Customer
    | Edit Customer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameInput name ->
            ( { model | name = name }, Cmd.none )

        SaveCustomer ->
            case model.id of
                Just id ->
                    ( model, editCustomer (Customer id model.name) )

                Nothing ->
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

        CustomerChanged changedCustomer ->
            let
                newCustomers =
                    List.map
                        (\customer ->
                            if customer.id == changedCustomer.id then
                                { id = customer.id, name = changedCustomer.name }

                            else
                                customer
                        )
                        model.customers

                -- Check if this is the customer we are currently editing,
                -- because another user might have edited another customer
                -- in the meantime.
                isCurrentlyEditedCustomer =
                    model.id == Just changedCustomer.id

                newName =
                    if isCurrentlyEditedCustomer then
                        ""

                    else
                        model.name

                newId =
                    if isCurrentlyEditedCustomer then
                        Nothing

                    else
                        model.id
            in
            ( { model
                | customers = newCustomers
                , name = newName
                , id = newId
              }
            , Cmd.none
            )

        Edit customer ->
            ( { model
                | id = Just customer.id
                , name = customer.name
              }
            , Cmd.none
            )



-- view


viewCustomer : Model -> Customer -> Html Msg
viewCustomer model customer =
    let
        shouldDisableEdit =
            case model.id of
                Just id ->
                    if id == customer.id then
                        False

                    else
                        True

                Nothing ->
                    False

        shouldEnableInput =
            case model.id of
                Just id ->
                    if id == customer.id then
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
            span
                []
                [ input
                    [ value model.name
                    , onInput NameInput
                    , type_ "text"
                    ]
                    []
                , button
                    [ type_ "button"
                    , onClick SaveCustomer
                    ]
                    [ text "Save" ]
                ]

          else
            span
                []
                [ text customer.name ]
        ]


viewCustomers : Model -> Html Msg
viewCustomers model =
    model.customers
        |> List.sortBy .id
        |> List.map (viewCustomer model)
        |> ul []


viewCustomerForm : Model -> Html Msg
viewCustomerForm model =
    let
        mainInputFieldValue =
            case model.id of
                Just id ->
                    ""

                Nothing ->
                    model.name
    in
    Html.form [ onSubmit SaveCustomer ]
        [ input [ type_ "text", onInput NameInput, value mainInputFieldValue ] []
        , text <| Maybe.withDefault "" model.error
        , button [ type_ "submit" ] [ text "Save" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Customer List" ]
        , viewCustomerForm model
        , viewCustomers model
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
        , customerChanged CustomerChanged
        ]



-- outgoing port


port addCustomer : String -> Cmd msg


port editCustomer : Customer -> Cmd msg


port deleteCustomer : Customer -> Cmd msg



-- incoming ports


port customerSaved : (String -> msg) -> Sub msg


port newCustomer : (Customer -> msg) -> Sub msg


port customerDeleted : (Customer -> msg) -> Sub msg


port customerChanged : (Customer -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = document
        , subscriptions = subscriptions
        }
