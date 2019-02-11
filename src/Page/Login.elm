port module Page.Login exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api exposing (Cred(..), httpErrorString)
import Api.Endpoint as Endpoint exposing (gotoMainsite, login, root_url)
import Avatar exposing (Avatar(..))
import Bootstrap.Button as Button exposing (onClick)
import Bootstrap.CDN as CDN
import Bootstrap.Modal as Modal exposing (..)
import Browser
import Browser.Navigation exposing (load)
import Html exposing (..)
import Html.AttributeBuilder as AB
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, float, int, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required, requiredAt)
import Json.Encode as Encode exposing (..)
import Route exposing (Route)
import Session exposing (Session)
import Username exposing (Username(..))
import Viewer exposing (Viewer(..))




type Message
    = Success
    | Error

type alias Errors =
    { username : String
    , password : String
    , non_field_errors : String
    }


type alias Model =
    { session : Session
    , modalVisibility : Modal.Visibility
    , container_active : Bool
    , username : String
    , ava : Maybe String
    , email : String
    , balance : Int
    , is_superuser : Bool
    , is_staff : Bool
    , is_active : Bool
    , token : String
    , password : String
    , passwordAgain : String
    , successmessages : List String
    , errorsMsg : List String
    , type_mess : Maybe Message
    
    }


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , modalVisibility = Modal.hidden
      , container_active = False
      , username = ""
      , ava = Just ""
      , email = ""
      , balance = 0
      , is_superuser = False
      , is_staff = False
      , is_active = False
      , token = ""
      , password = ""
      , passwordAgain = ""
      , successmessages = []
      , errorsMsg = []
      , type_mess = Nothing
      }
    , Cmd.none
    )


usersignupEncoder : Model -> Encode.Value
usersignupEncoder model =
    Encode.object
        [ ( "username", Encode.string model.username )
        , ( "email", Encode.string model.email )
        , ( "password1", Encode.string model.password )
        , ( "password2", Encode.string model.passwordAgain )
        ]


signupUser : Model -> Cmd Msg
signupUser model =
    let
        bod =
            Encode.object
                [ ( "username", Encode.string model.username )
                , ( "email", Encode.string model.email )
                , ( "password1", Encode.string model.password )
                , ( "password2", Encode.string model.passwordAgain )
                ]
                |> Http.jsonBody
    in
    signupDecoder
        |> Api.post Endpoint.signup_url Nothing bod
        |> Http.send GetTokenCompleted


signupDecoder : Decoder String
signupDecoder =
    Decode.field "key" Decode.string


authUser : Model -> Cmd Msg
authUser model =
    let
        bod =
            Encode.object
                [ ( "username", Encode.string model.username )
                , ( "password", Encode.string model.password )
                ]
                |> Http.jsonBody
    in
    loginDecoder
        |> Api.post Endpoint.login_url Nothing bod
        |> Http.send GetTokenCompleted


loginDecoder : Decoder String
loginDecoder =
    Decode.field "token" Decode.string


getTokenCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case result of
        Ok some_token ->
            let
                withToken =
                    { model
                        | token = some_token
                        , successmessages = [ "thành công" ]
                        , modalVisibility = Modal.shown
                        , type_mess = Just Success
                    }
            in
            ( model
            , Viewer.store <| convert <| withToken
            )

        Err error ->
            ( { model
                | errorsMsg = ["Hãy chắc chắn rằng bạn đã nhập chính xác.","Nếu như bạn đang đăng kí mới tài khoản. Có lẽ đã có người sử dụng.", httpErrorString error ]
                , modalVisibility = Modal.shown
                , type_mess = Just Error
              }
            , Cmd.none
            )


convert : Model -> Viewer
convert model =
    let
        uname =
            Username model.username

        ava =
            Avatar model.ava

        cred =
            Cred uname model.token
    in
    Viewer ava cred


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Session.changes GotSession (Session.navKey model.session)
        , Modal.subscriptions model.modalVisibility AnimateModal
        ]



-- UPDATE


type Msg
    = SetUsername String
    | SetPassword String
    | SetPasswordAgain String
    | SetEmail String
    | ClickLogin
    | ClickSignup
    | GetTokenCompleted (Result Http.Error String)
    | Addclass_active
    | Addclass_close
    | GotSession Session
    | CloseModal
    | ShowModal
    | AnimateModal Modal.Visibility


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session, errorsMsg = [] }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        ClickLogin ->
            ( { model | successmessages = [], errorsMsg = [] }
            , authUser model
            )

        ClickSignup ->
            ( { model
                | successmessages = []
                , errorsMsg = []
              }
            , signupUser model
            )

        SetUsername name ->
            ( { model
                | username = name
              }
            , Cmd.none
            )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        SetPasswordAgain password ->
            ( { model | passwordAgain = password }, Cmd.none )

        GetTokenCompleted result ->
            getTokenCompleted model result

        Addclass_active ->
            ( { model | container_active = True }, Cmd.none )

        Addclass_close ->
            ( { model | container_active = False }, Cmd.none )

        ShowModal ->
            ( { model | modalVisibility = Modal.shown }, Cmd.none )

        AnimateModal visibility ->
            ( { model | modalVisibility = visibility }
            , Cmd.none
            )

        CloseModal ->
            ( { model
                | modalVisibility = Modal.hidden
                , successmessages = []
                , errorsMsg = []
              }
            , Cmd.none
            )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Trang tài khoản"
    , content = login_page model
    }


stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "static/css/Account/login.css"
            ]

        children =
            []
    in
    node tag attrs children


login_page : Model -> Html Msg
login_page model =
    div
        [ Html.Attributes.classList
            [ ( "container", True )
            , ( "active", model.container_active )
            ]
        ]
        [ -- stylesheet
          div [ class "card" ] []
        , div [ class "card" ]
            [ Html.h1 [ class "title" ]
                [ text "Đăng Nhập" ]
            , div []
                [ div [ class "input-container" ]
                    [ input
                        [ Html.Attributes.value model.username
                        , onInput SetUsername
                        , attribute "required" "required"
                        , autocomplete False
                        ]
                        []
                    , label [ for "username" ]
                        [ text "Tài Khoản" ]
                    , div [ class "bar" ]
                        []
                    ]
                , div [ class "input-container" ]
                    [ input
                        [ type_ "password"
                        , onInput SetPassword
                        , attribute "required" "required"
                        , autocomplete False
                        ]
                        []
                    , label [ for "pass" ]
                        [ text "Mật Khẩu" ]
                    , div [ class "bar" ]
                        []
                    ]
                , div [ class "button-container" ]
                    [ button
                        [ if String.length model.username > 0 && String.length model.password > 0 then
                            onClick ClickLogin

                          else
                            class ""
                        ]
                        [ span []
                            [ text "Đăng Nhập" ]
                        ]
                    ]
                , div [ class "footer" ]
                    [ a [ href "#" ]
                        [ text "Forgot your password?" ]
                    ]
                ]
            ]
        , div [ class "card alt" ]
            [ div
                [ class "toggle", onClick Addclass_active ]
                [ text "" ]
            , Html.h1 [ class "title" ]
                [ text "Đăng Ký"
                , div [ class "close", onClick Addclass_close ]
                    []
                ]
            , div []
                [ div [ class "input-container" ]
                    [ input
                        [ id "username"
                        , attribute "required" "required"
                        , autocomplete False
                        , onInput SetUsername
                        ]
                        []
                    , label [ for "username" ]
                        [ text "Tên Tài Khoản" ]
                    , div [ class "bar" ]
                        []
                    ]
                , div [ class "input-container" ]
                    [ input
                        [ id "email"
                        , attribute "required" "required"
                        , autocomplete False
                        , onInput SetEmail
                        ]
                        []
                    , label [ for "email" ]
                        [ text "E-mail" ]
                    , div [ class "bar" ]
                        []
                    ]
                , div [ class "input-container" ]
                    [ input
                        [ type_ "password"
                        , id "pass"
                        , attribute "required" "required"
                        , autocomplete False
                        , onInput SetPassword
                        ]
                        []
                    , label [ for "pass" ]
                        [ text "Mật Khẩu" ]
                    , div [ class "bar" ]
                        []
                    ]
                , div [ class "input-container" ]
                    [ input
                        [ type_ "password"
                        , id "repass"
                        , attribute "required" "required"
                        , autocomplete False
                        , onInput SetPasswordAgain
                        ]
                        []
                    , label [ for "repass" ]
                        [ text "Nhập lại Mật Khẩu" ]
                    , div [ class "bar" ]
                        []
                    ]
                , div [ class "button-container" ]
                    [ button [ onClick ClickSignup ]
                        [ span []
                            [ text "Đăng Ký" ]
                        ]
                    ]
                ]
            ]
        , modalView model
        ]


proFile : Model -> Html Msg
proFile model =
    div [] []


toSession : Model -> Session
toSession model =
    model.session


modalView : Model -> Html Msg
modalView model =
    case model.type_mess of
        Nothing ->
            Modal.config CloseModal
                |> Modal.h5 [] [ text "Lỗi không xác định" ]
                |> Modal.body [] [ view_list_mes model.errorsMsg ]
                |> Modal.footer []
                    [ Button.button
                        [ Button.outlinePrimary
                        , Button.attrs [ onClick <| AnimateModal Modal.hiddenAnimated ]
                        ]
                        [ text "Close" ]
                    ]
                |> Modal.view model.modalVisibility

        Just Error ->
            Modal.config CloseModal
                |> Modal.header [class "modal-header badge badge-danger"][Html.h5 [] [ text "Lỗi" ]]
                |> Modal.body [] [ view_list_mes model.errorsMsg ]
                |> Modal.footer []
                    [ Button.button
                        [ Button.outlinePrimary
                        , Button.attrs [ onClick <| AnimateModal Modal.hiddenAnimated ]
                        ]
                        [ text "Close" ]
                    ]
                |> Modal.view model.modalVisibility

        Just Success ->
            Modal.config CloseModal
                |> Modal.header [class "modal-header badge badge-success"][Html.h5 [] [ text "Thành Công" ]]
                
                |> Modal.body [] [ view_list_mes model.successmessages ]
                |> Modal.footer []
                    [ Button.button
                        [ Button.outlinePrimary
                        , Button.attrs [ onClick <| AnimateModal Modal.hiddenAnimated ]
                        ]
                        [ text "Ẩn" ]
                    ]
                |> Modal.view model.modalVisibility


view_list_mes : List String -> Html Msg
view_list_mes errors =
    div [] <| List.map single_mess errors


single_mess : String -> Html Msg
single_mess error =
    p [] [ text <| error ]
