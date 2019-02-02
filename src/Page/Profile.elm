module Page.Profile exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred(..), httpErrorString)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Bootstrap.Button as Button exposing (onClick)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid exposing (..)
import Bootstrap.Grid.Col as Col exposing (..)
import Bootstrap.Modal as Modal exposing (..)
import Bootstrap.Tab as Tab exposing (..)
import Bootstrap.Table as Table
import Browser.Navigation as Nav
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (spanishLocale, usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, float, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required, requiredAt)
import Json.Encode as Encode exposing (..)
import Route exposing (..)
import Session exposing (Session)
import Username exposing (Username)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Tab.subscriptions model.tabState TabMsg
        , Modal.subscriptions model.modalVisibility AnimateModal
        ]


toSession : Model -> Session
toSession model =
    model.session


type alias Model =
    { session : Session
    , title : String
    , user : User
    , modalVisibility : Modal.Visibility
    , tabState : Tab.State
    , usernamedbank : String
    , userbankname : String
    , userbanknumber : String
    , add_bank_message_successed : Bool
    , errorMsg : String
    , errorsMsg : List String
    , form_add_bank : Bool
    }


type alias Bank =
    { name : String
    , bank_name : String
    , bank_num : String
    , is_activated : String
    }


type alias User =
    { username : String
    , balance : Int
    , email : String
    , banks : List Bank
    }



-- EXPORT


init : Session -> ( Model, Cmd Msg )
init session =
    let
        maybeCred =
            Session.cred session
    in
    ( { session = session
      , user =
            { username = ""
            , balance = 0
            , email = ""
            , banks =
                [ { name = ""
                  , bank_name = ""
                  , bank_num = ""
                  , is_activated = ""
                  }
                ]
            }
      , title = "Trang cá nhân"
      , modalVisibility = Modal.hidden
      , tabState = Tab.initialState
      , usernamedbank = ""
      , userbankname = ""
      , userbanknumber = ""
      , add_bank_message_successed = False
      , errorMsg = ""
      , errorsMsg = []
      , form_add_bank = False
      }
    , Api.get Endpoint.user (Session.cred session) (Decode.field "user" formDecoder)
        |> Http.send FetchMe
    )


type Msg
    = NoOp
    | TabMsg Tab.State
    | FetchMe (Result Http.Error User)
    | ShowFormAddBank
    | SetNameForBank String
    | SetBankName String
    | SetBankNumber String
    | SubmmitBank Cred
    | AddBankResult (Result Http.Error Bool)
    | AnimateModal Modal.Visibility
    | CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimateModal visibility ->
            ( { model | modalVisibility = visibility }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }
            , Cmd.none
            )

        AddBankResult (Ok val) ->
            ( { model
                | add_bank_message_successed = val
                , errorsMsg = [ "e1", "e2" ]
                , modalVisibility = Modal.shown
              }
              -- |> Debug.log "check modal: "
              --   ,Cmd.none
            , Cmd.batch [ Nav.load <| routeToString <| Route.Profile ]
            )

        AddBankResult (Err val) ->
            ( { model | errorMsg = httpErrorString val }, Cmd.none )

        SubmmitBank cred ->
            ( model, addBank model cred )

        SetNameForBank vl ->
            ( { model | usernamedbank = vl }, Cmd.none )

        SetBankName vl ->
            ( { model | userbankname = vl }, Cmd.none )

        SetBankNumber vl ->
            ( { model | userbanknumber = vl }, Cmd.none )

        ShowFormAddBank ->
            ( { model | form_add_bank = True }, Cmd.none )

        TabMsg state ->
            ( { model | tabState = state }
            , Cmd.none
            )

        FetchMe (Err val) ->
            ( { model | errorMsg = httpErrorString val }, Cmd.none )

        FetchMe (Ok val) ->
            -- Debug.log "load ok"
            let
                user =
                    model.user

                newuser =
                    { user
                        | username = val.username
                        , balance = val.balance
                        , email = val.email
                        , banks = val.banks
                    }
            in
            ( { model | user = newuser }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


addBank : Model -> Cred -> Cmd Msg
addBank model cred =
    let
        bod =
            Encode.object
                [ ( "name", Encode.string model.usernamedbank )
                , ( "bank_name", Encode.string model.userbankname )
                , ( "bank_num", Encode.string model.userbanknumber )
                ]
                |> Http.jsonBody
    in
    addBankDecoder
        |> Api.post Endpoint.addBank_url (Just cred) bod
        |> Http.send AddBankResult


addBankDecoder : Decoder Bool
addBankDecoder =
    Decode.field "successed" Decode.bool


profile_auth_content : Model -> Cred -> Html Msg
profile_auth_content model cred =
    main_
        [ class "mdl-layout__content"
        ]
        [ stylesheet
        , div [ class "page-content" ]
            [ div [ class "card" ] []
            , div [ class "card" ]
                [ Html.h1 [ class "title" ] [ text model.title ]
                , tab model cred
                ]
            ]
        ]


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Trang cá nhân"
    , content =
        case Session.cred model.session of
            Just cred ->
                profile_auth_content model cred

            Nothing ->
                text "Sign in to edit this article."
    }


tab : Model -> Cred -> Html Msg
tab model cred =
    case model.form_add_bank of
        True ->
            div [ class "add-bank text-center" ]
                [ Html.h4 [ class "title" ] [ text model.errorMsg ]
                , div [ class "input-container" ]
                    [ input
                        [ Html.Attributes.value model.usernamedbank
                        , onInput SetNameForBank
                        , attribute "namedbank" "required"
                        , autocomplete False
                        ]
                        []
                    , label [ for "namedbank" ]
                        [ text "Tên (tuỳ thích )" ]
                    , div [ class "bar" ]
                        []
                    ]
                , div [ class "input-container" ]
                    [ input
                        [ Html.Attributes.value model.userbankname
                        , onInput SetBankName
                        , attribute "bankname" "required"
                        , autocomplete False
                        ]
                        []
                    , label [ for "bankname" ]
                        [ text "Tên ngân hàng" ]
                    , div [ class "bar" ]
                        []
                    ]
                , div [ class "input-container" ]
                    [ input
                        [ Html.Attributes.value model.userbanknumber
                        , onInput SetBankNumber
                        , attribute "banknum" "required"
                        , autocomplete False
                        ]
                        []
                    , label [ for "banknum" ]
                        [ text "Số tài khoản" ]
                    , div [ class "bar" ]
                        []
                    ]
                , div [ class "button-container" ]
                    [ Html.button
                        [ class
                            (if String.length model.userbankname > 0 && String.length model.userbanknumber > 0 && String.length model.usernamedbank > 0 then
                                "valid"

                             else
                                ""
                            )
                        , onClick
                            (SubmmitBank cred)
                        ]
                        [ span []
                            [ text "Thêm ngân hàng" ]
                        ]
                    ]
                ]

        False ->
            div [ class "" ]
                [ Tab.config TabMsg
                    |> Tab.justified
                    |> Tab.pills
                    |> Tab.right
                    |> Tab.items
                        [ Tab.item
                            { id = "detail"
                            , link =
                                Tab.link [ class "button-container" ]
                                    [ div []
                                        [ Html.button []
                                            [ span []
                                                [ text "Thông tin" ]
                                            ]
                                        ]
                                    ]
                            , pane = Tab.pane [] [ hr [] [], info_tab model.user cred ]
                            }
                        , Tab.item
                            { id = "trans"
                            , link =
                                Tab.link [ class "button-container" ]
                                    [ div []
                                        [ Html.button []
                                            [ span []
                                                [ text "Giao dịch" ]
                                            ]
                                        ]
                                    ]
                            , pane = Tab.pane [] [ hr [] [], trans_tab model ]
                            }
                        ]
                    |> Tab.view model.tabState
                ]


stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "static/css/Account/profile.css"
            ]

        children =
            []
    in
    node tag attrs children


info_tab : User -> Cred -> Html Msg
info_tab user cred =
    div []
        [ div [ class "row" ]
            [ div [ class "col-sm-4" ]
                [ div []
                    [ label [] [ text "Tên đăng nhập: " ]
                    , strong [] [ text user.username ]
                    ]
                , div []
                    [ label [] [ text "Email: " ]
                    , strong [] [ text user.email ]
                    ]
                , div []
                    [ label [] [ text "Số dư: " ]
                    , strong []
                        [ user.balance
                            |> toFloat
                            |> format { usLocale | decimals = 0, thousandSeparator = "." }
                            |> append_balance " (VND)"
                            |> text
                        ]
                    ]
                ]
            , div [ class "col-sm-8 banks" ]
                [ listbankUser user.banks cred
                ]
            ]
        ]


append_balance : String -> String -> String
append_balance str str1 =
    str1 ++ str


listbankUser : List Bank -> Cred -> Html Msg
listbankUser banks cred =
    ul []
        [ li [ class "list-bank" ]
            [ aside
                [ class "bank-card"
                , onClick ShowFormAddBank
                ]
                [ strong
                    [ class ""
                    ]
                    [ text "" ]
                , div [ class "bank-detail" ]
                    [ strong [ style "color" "#e82953" ] [ text "Thêm ngân hàng " ] ]
                ]
            ]
        , div [] <| List.map singlebank banks
        ]


singlebank : Bank -> Html msg
singlebank bank =
    li [ class "list-bank" ]
        [ aside
            [ class
                (if String.length bank.is_activated > 0 then
                    "bank-card verified"

                 else
                    "bank-card"
                )
            ]
            [ strong [ class "bank-title" ]
                [ text bank.name ]
            , div [ class "bank-detail" ]
                [ p [] [ text ("Bank name: " ++ bank.bank_name) ]
                , p [] [ text ("Bank number: " ++ bank.bank_num) ]
                ]
            ]
        ]


trans_tab : Model -> Html msg
trans_tab model =
    Table.simpleTable
        ( Table.simpleThead
            [ Table.th [] [ Html.text "col1" ]
            , Table.th [] [ Html.text "col2" ]
            ]
        , Table.tbody []
            [ Table.tr []
                [ Table.td [] [ Html.text "col1row1" ]
                , Table.td [] [ Html.text "col2row1" ]
                ]
            , Table.tr []
                [ Table.td [] [ Html.text "col1row2" ]
                , Table.td [] [ Html.text "col2row2" ]
                ]
            ]
        )


formDecoder : Decoder User
formDecoder =
    Decode.succeed User
        |> required "username" Decode.string
        |> required "balance" Decode.int
        |> required "email" Decode.string
        |> required "bank" decodeListBank


decodeListBank : Decoder (List Bank)
decodeListBank =
    Decode.list bankDecode


bankDecode : Decoder Bank
bankDecode =
    Decode.succeed Bank
        |> required "name" Decode.string
        |> required "bank_name" Decode.string
        |> required "bank_num" Decode.string
        |> required "is_activated" Decode.string


modalview : Model -> Html Msg
modalview model =
    case List.length model.errorsMsg of
        0 ->
            div [ class "non error" ] []

        _ ->
            Grid.container [ class "errors" ]
                [ Modal.config CloseModal
                    |> Modal.h5 [] [ text "Modal header" ]
                    |> Modal.body [] [ view_list_errors model.errorsMsg ]
                    |> Modal.footer []
                        [ Button.button
                            [ Button.outlinePrimary, Button.onClick <| AnimateModal Modal.hiddenAnimated ]
                            [ text "Close" ]
                        ]
                    |> Modal.view model.modalVisibility
                ]


view_list_errors : List String -> Html Msg
view_list_errors errors =
    div [] <| List.map single_error errors


single_error : String -> Html Msg
single_error error =
    p [] [ text <| error ]
