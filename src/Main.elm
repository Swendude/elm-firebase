port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, img, input, p, text)
import Html.Attributes exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


port signIn : () -> Cmd msg


port registerUser : Json.Encode.Value -> Cmd msg


port signInInfo : (Json.Encode.Value -> msg) -> Sub msg


port signInError : (Json.Encode.Value -> msg) -> Sub msg


port signOut : () -> Cmd msg


port saveGroup : Json.Encode.Value -> Cmd msg


port receiveGroups : (Json.Encode.Value -> msg) -> Sub msg



---- MODEL ----


type alias ErrorData =
    { code : Maybe String
    , message : Maybe String
    , credential : Maybe String
    }


type alias UserData =
    { token : String
    , email : String
    , uid : String
    }


type alias Member =
    { name : String
    , class : String
    }


type alias GroupData =
    { name : String
    , members : Maybe (List Member)
    }


type alias Model =
    { userData : Maybe UserData
    , groups : Maybe (List GroupData)
    , selectedGroup : Maybe Int
    , error : ErrorData
    , inputContent : String
    }


init : ( Model, Cmd Msg )
init =
    ( { userData = Maybe.Nothing, groups = Maybe.Nothing, selectedGroup = Maybe.Nothing, error = emptyError, inputContent = "" }, Cmd.none )



-- Collections -> Documents/Sub-collection
-- Collection: User
-- Name : String
-- UID : Int
-- Password: String
---- UPDATE ----


type Msg
    = LogIn
    | LogOut
    | LoggedInData (Result Json.Decode.Error UserData)
    | LoggedInError (Result Json.Decode.Error ErrorData)
    | SaveGroup
    | InputChanged String
    | GroupsReceived (Result Json.Decode.Error (List String))
    | ChangeGroup Int


emptyError : ErrorData
emptyError =
    { code = Maybe.Nothing, credential = Maybe.Nothing, message = Maybe.Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogIn ->
            ( model, signIn () )

        LogOut ->
            ( { model | userData = Maybe.Nothing, error = emptyError }, signOut () )

        LoggedInData result ->
            case result of
                Ok value ->
                    ( { model | userData = Just value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        LoggedInError result ->
            case result of
                Ok value ->
                    ( { model | error = value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        SaveGroup ->
            ( model, saveGroup <| messageEncoder model )

        InputChanged value ->
            ( { model | inputContent = value }, Cmd.none )

        GroupsReceived result ->
            case result of
                Ok value ->
                    ( { model | groups = Just <| List.map (\s -> { name = s, members = Nothing }) value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        ChangeGroup n ->
            ( { model | selectedGroup = Just n }, registerUser <| userEncoder )


messageEncoder : Model -> Json.Encode.Value
messageEncoder model =
    Json.Encode.object
        [ ( "content", Json.Encode.string model.inputContent )
        , ( "uid"
          , case model.userData of
                Just userData ->
                    Json.Encode.string userData.uid

                Maybe.Nothing ->
                    Json.Encode.null
          )
        ]


userEncoder : Json.Encode.Value
userEncoder =
    Json.Encode.object
        [ ( "email", Json.Encode.string "Swenmulderij@gmail.com" )
        , ( "password", Json.Encode.string "safesafe" )
        ]


messageToError : String -> ErrorData
messageToError message =
    { code = Maybe.Nothing, credential = Maybe.Nothing, message = Just message }


errorPrinter : ErrorData -> String
errorPrinter errorData =
    Maybe.withDefault "" errorData.code ++ " " ++ Maybe.withDefault "" errorData.credential ++ " " ++ Maybe.withDefault "" errorData.message


userDataDecoder : Json.Decode.Decoder UserData
userDataDecoder =
    Json.Decode.succeed UserData
        |> Json.Decode.Pipeline.required "token" Json.Decode.string
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
        |> Json.Decode.Pipeline.required "uid" Json.Decode.string


logInErrorDecoder : Json.Decode.Decoder ErrorData
logInErrorDecoder =
    Json.Decode.succeed ErrorData
        |> Json.Decode.Pipeline.required "code" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "message" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "credential" (Json.Decode.nullable Json.Decode.string)


messagesDecoder =
    Json.Decode.decodeString (Json.Decode.list Json.Decode.string)


messageListDecoder : Json.Decode.Decoder (List String)
messageListDecoder =
    Json.Decode.succeed identity
        |> Json.Decode.Pipeline.required "messages" (Json.Decode.list Json.Decode.string)



---- VIEW ----


groupView : Int -> Int -> GroupData -> Html Msg
groupView selected i group =
    if i /= selected then
        Html.li []
            [ Html.div []
                [ Html.text group.name
                , Html.button [ onClick <| ChangeGroup i ] [ Html.text "Select" ]
                ]
            ]

    else
        Html.li []
            [ Html.div []
                [ Html.text group.name
                ]
            ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Lootshare" ]
        , case model.userData of
            Just data ->
                button [ onClick LogOut ] [ text "Logout from Google" ]

            Maybe.Nothing ->
                button [ onClick LogIn ] [ text "Login with Google" ]
        , h2 []
            [ text <|
                case model.groups of
                    Nothing ->
                        "No groups found"

                    Just groups ->
                        "There are groups"
            ]
        , case model.groups of
            Nothing ->
                Html.div [] []

            Just groups ->
                Html.ul [] <|
                    List.indexedMap (groupView (Maybe.withDefault 0 model.selectedGroup)) groups
        , case model.userData of
            Just data ->
                div []
                    [ input [ placeholder "Group to create", value model.inputContent, onInput InputChanged ] []
                    , button [ onClick SaveGroup ] [ text "Create group" ]
                    ]

            Nothing ->
                div [] []
        , h2 [] [ text <| errorPrinter model.error ]
        ]



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ signInInfo (Json.Decode.decodeValue userDataDecoder >> LoggedInData)
        , signInError (Json.Decode.decodeValue logInErrorDecoder >> LoggedInError)
        , receiveGroups (Json.Decode.decodeValue messageListDecoder >> GroupsReceived)
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
