module Page.Profile exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred(..), httpErrorString)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Bootstrap.Button as Button exposing (onClick)
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


type Message
    = Success
    | Error


type alias Model =
    { session : Session
    , headline : String
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
    , form_withdraw : Bool
    , withdraw_amount : Maybe Int
    , select_bank_withdraw : String
    , successmessages : List String
    , type_mess : Maybe Message
    }


type alias Bank =
    { name : String
    , bank_name : String
    , bank_num : String
    , is_activated : String
    , domaintagbank : String
    }


type alias Trans =
    { from_user : String
    , to_user : String
    , value : Int
    , date_create : String
    }


type alias User =
    { username : String
    , balance : Int
    , email : String
    , banks : List Bank
    , trans : List Trans
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
                  , domaintagbank = ""
                  }
                ]
            , trans =
                [ { from_user = ""
                  , to_user = ""
                  , value = 0
                  , date_create = ""
                  }
                ]
            }
      , headline = "Trang cá nhân"
      , modalVisibility = Modal.hidden
      , tabState = Tab.initialState
      , usernamedbank = ""
      , userbankname = ""
      , userbanknumber = ""
      , add_bank_message_successed = False
      , errorMsg = ""
      , errorsMsg = []
      , form_add_bank = False
      , form_withdraw = False
      , withdraw_amount = Nothing
      , select_bank_withdraw = ""
      , successmessages = []
      , type_mess = Nothing
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
    | Show_WithDraw_Form
    | SetSelectedBank String
    | Back
    | ShowModal
    | SetWithDrawAmount String
    | WithDrawCmd Cred
    | WithDrawResult (Result Http.Error Bool)
    | FormValid


cashout : Model -> Cred -> Cmd Msg
cashout model cred =
    let
        bod =
            Encode.object
                [ ( "bank", Encode.string model.select_bank_withdraw )
                , ( "value", Encode.int <| Maybe.withDefault 0 <| model.withdraw_amount )
                ]
                |> Http.jsonBody
    in
    withdrawDecoder
        |> Api.post Endpoint.cashout_url (Just cred) bod
        |> Http.send WithDrawResult


withdrawDecoder : Decoder Bool
withdrawDecoder =
    Decode.at [ "results", "successed" ] Decode.bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormValid ->
            ( { model
                | errorsMsg = [ "Hãy chắc chắn bạn đã lựa chọn ngân hàng và số tiền muốn rút" ]
                , type_mess = Just Error
                , modalVisibility = Modal.shown
              }
            , Cmd.none
            )

        WithDrawResult (Ok val) ->
            case val of
                True ->
                    let
                        olduser =
                            model.user

                        newuser =
                            { olduser
                                | balance = olduser.balance - (Maybe.withDefault 0 <| model.withdraw_amount)
                            }
                    in
                    ( { model
                        | user = newuser
                        , successmessages = [ "Rút tiền thành công. Giao dịch của bạn đang được xử lý. Hãy kiểm tra tại phần giao dịch" ]
                        , type_mess = Just Success
                        , modalVisibility = Modal.shown
                        , withdraw_amount = Nothing
                        , select_bank_withdraw = ""
                        , form_withdraw = False
                      }
                    , Cmd.none
                    )

                False ->
                    ( model, Cmd.none )

        WithDrawResult (Err err) ->
            ( { model
                | errorsMsg = [ httpErrorString err ]
                , type_mess = Just Error
                , modalVisibility = Modal.shown
                , withdraw_amount = Nothing
              }
            , Cmd.none
            )

        WithDrawCmd cred ->
            ( model, cashout model cred )

        SetSelectedBank str ->
            ( { model | select_bank_withdraw = str }, Cmd.none )

        SetWithDrawAmount val ->
            case String.toInt <| val of
                Just int ->
                    let
                        user =
                            model.user
                    in
                    if int <= user.balance then
                        ( { model
                            | withdraw_amount = Just int
                          }
                        , Cmd.none
                        )

                    else
                        let
                            err =
                                "Số dư của bạn không đủ. Bạn chỉ có thể rút được số tiền nhỏ hơn hoặc bằng: "
                                    ++ (user.balance
                                            |> toFloat
                                            |> format { usLocale | decimals = 0, thousandSeparator = "." }
                                       )
                        in
                        ( { model
                            | errorsMsg = [ err ]
                            , withdraw_amount = Nothing
                            , type_mess = Just Error
                            , modalVisibility = Modal.shown
                            
                          }
                        , Cmd.none
                        )

                Nothing ->
                    -- ( { model | modalVisibility = Modal.shown }, Cmd.none )
                    ( model, Cmd.none )

        Show_WithDraw_Form ->
            ( { model | form_withdraw = True, headline = "Quay lại" }, Cmd.none )

        AnimateModal visibility ->
            ( { model | modalVisibility = visibility }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden, errorMsg = "", errorsMsg = [] }
            , Cmd.none
            )

        AddBankResult (Ok val) ->
            let
                user =
                    model.user

                newuser =
                    { user
                        | balance =
                            user.balance
                                - (Maybe.withDefault 0 <|
                                    model.withdraw_amount
                                  )
                    }
            in
            ( { model
                | user = newuser
                , successmessages =
                    [ "Thêm ngân hàng thành công. Bạn hãy liên hệ với quản trị để xác thực ngân hàng."
                    , "Hoặc Sử dụng hệ thống xác nhận tự động, bằng cách chuyển khoản 10.000 (vnd) đến STK: XXX "
                    , "Nội dụng chuyển khoản là: ' " ++ user.username ++ " " ++ model.usernamedbank ++ " Xac thuc '"
                    ]
                , type_mess = Just Success
                , modalVisibility = Modal.shown
                , form_add_bank = False
              }
            , Cmd.none
              -- batch [ Nav.load <| routeToString <| Route.Profile ]
            )

        AddBankResult (Err val) ->
            ( { model
                | errorsMsg = [ httpErrorString val ]
                , modalVisibility = Modal.shown
              }
            , Cmd.none
            )

        SubmmitBank cred ->
            ( model, addBank model cred )

        SetNameForBank vl ->
            ( { model | usernamedbank = vl }, Cmd.none )

        SetBankName vl ->
            ( { model | userbankname = vl }, Cmd.none )

        SetBankNumber vl ->
            ( { model | userbanknumber = vl }, Cmd.none )

        ShowFormAddBank ->
            ( { model | form_add_bank = True, headline = "Quay lại" }, Cmd.none )

        TabMsg state ->
            ( { model | tabState = state }
            , Cmd.none
            )

        FetchMe (Err val) ->
            ( { model | errorMsg = httpErrorString val }, Cmd.none )

        FetchMe (Ok val) ->
            let
                user =
                    model.user

                newuser =
                    { user
                        | username = val.username
                        , balance = val.balance
                        , email = val.email
                        , banks = val.banks
                        , trans = val.trans
                    }
            in
            ( { model | user = newuser }, Cmd.none )

        Back ->
            ( { model | errorMsg = "", errorsMsg = [], form_add_bank = False, form_withdraw = False, headline = "Trang cá nhân" }, Cmd.none )

        ShowModal ->
            ( { model | modalVisibility = Modal.shown }, Cmd.none )

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
                [ Html.h1
                    [ class "title btn"
                    , onClick Back
                    , style "text-align" "left !important"
                    ]
                    [ model.headline |> text ]
                , tab model cred
                , modalView model
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
                div [ class "container" ] [ text "Sign in to edit this article." ]
    }



-- all form on this page will be apeal here


tab : Model -> Cred -> Html Msg
tab model cred =
    if model.form_withdraw == True then
        div []
            [ withdraw model cred ]

    else if model.form_add_bank == True then
        div []
            [ add_bank_form model cred
            ]

    else
        normal_show model cred


add_bank_form : Model -> Cred -> Html Msg
add_bank_form model cred =
    div [ class "add-bank text-center container" ]
        [ Html.h5 [ class "subtitle", style "margin-bottom" "50px" ] [ strong [] [ text "Thêm ngân hàng" ] ]
        , div [ class "input-container form-group" ]
            [ input
                [ Html.Attributes.value model.usernamedbank
                , onInput SetNameForBank
                , attribute "namedbank" "required"
                , autocomplete False
                ]
                []
            , label [ for "namedbank" ]
                [ text "Tên (tuỳ chọn )" ]
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


normal_show : Model -> Cred -> Html Msg
normal_show model cred =
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
                    , pane = Tab.pane [] [ hr [] [], info_tab model.user cred model ]
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


info_tab : User -> Cred -> Model -> Html Msg
info_tab user cred model =
    div []
        [ div [ class "row" ]
            [ div [ class "col-sm-4" ]
                [ Html.h5 [ class "subtitle" ] [ strong [] [ text "Thông tin người dùng" ] ]
                , div [ class "container user-info" ]
                    [ div []
                        [ label [] [ text "Tên đăng nhập: " ]
                        , strong [] [ text <| user.username ]
                        ]
                    , div []
                        [ label [] [ text "Email: " ]
                        , strong [] [ text user.email ]
                        ]
                    , div []
                        [ label [] [ text "Số dư: " ]
                        , strong []
                            [ user.balance
                                - (Maybe.withDefault 0 <| model.withdraw_amount)
                                |> toFloat
                                |> format { usLocale | decimals = 0, thousandSeparator = "." }
                                |> append_balance " (VND)"
                                |> text
                            ]
                        ]
                    , div [ class "" ]
                        [ Html.button
                            [ class "btn btn-danger btn-lg btn-block"
                            , Html.Events.onClick Show_WithDraw_Form
                            ]
                            [ text "Rút tiền"
                            ]
                        ]
                    ]
                ]
            , div [ class "col-sm-8 banks" ]
                [ Html.h5 [ class "subtitle" ] [ strong [] [ text "Ngân hàng" ] ]
                , div [ class "user-info" ]
                    [ listbankUser user.banks cred ]
                ]
            ]
        ]


append_balance : String -> String -> String
append_balance str str1 =
    str1 ++ str


listbankUser : List Bank -> Cred -> Html Msg
listbankUser banks cred =
    ul []
        [ li [ class "list-bank btn" ]
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
                [ p [class "bankdetailtxt"] [ text ("Bank name: " ++ bank.bank_name) ]
                , p [class "bankdetailtxt"] [ text ("Bank number: " ++ bank.bank_num) ]
                ]
            ]
        ]


trans_tab : Model -> Html msg
trans_tab model =
    let
        user =
            model.user

        trans =
            user.trans
    in
    div []
        [ Html.h5 [ class "subtitle" ] [ strong [] [ text "Giao dịch" ] ]
        , Table.simpleTable
            ( Table.simpleThead
                [ Table.th [] [ Html.text "Người chuyển" ]
                , Table.th [] [ Html.text "Người nhận" ]
                , Table.th [] [ Html.text "Trị giá (VND)" ]
                , Table.th [] [ Html.text "Thời gian" ]
                ]
            , Table.tbody [] <| List.map singletranview trans
              -- , Table.tr []
              --     [ Table.td [] [ Html.text "col1row2" ]
              --     , Table.td [] [ Html.text "col2row2" ]
              --     ]
            )
        ]


singletranview : Trans -> Table.Row msg
singletranview tran =
    Table.tr []
        [ Table.td [] [ Html.text tran.from_user ]
        , Table.td [] [ Html.text tran.to_user ]
        , Table.td []
            [ tran.value
                |> toFloat
                |> format { usLocale | decimals = 0, thousandSeparator = "." }
                |> Html.text
            ]
        , Table.td [] [ Html.text <| tran.date_create ]
        ]


formDecoder : Decoder User
formDecoder =
    Decode.succeed User
        |> required "username" Decode.string
        |> required "balance" Decode.int
        |> required "email" Decode.string
        |> required "bank" decodeListBank
        |> required "transaction" decodeListTran


decodeListTran : Decoder (List Trans)
decodeListTran =
    Decode.list transDecode


transDecode : Decoder Trans
transDecode =
    Decode.succeed Trans
        |> required "from_user" Decode.string
        |> required "to_user" Decode.string
        |> required "value" Decode.int
        |> required "date_create" Decode.string


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
        |> required "domaintagbank" Decode.string


withdraw : Model -> Cred -> Html Msg
withdraw model cred =
    let
        user =
            model.user
    in
    div [ class "withdraw text-center container" ]
        [ Html.h5 [ class "subtitle" ] [ strong [] [ text "Rút Tiền" ] ]
        , div [ class "input-container form-group" ]
            [ label [ for "selectbank", style "position" "relative" ]
                [ text "Lựa chọn ngân hàng" ]
            , selectview model.user
            ]
        , div [ class "input-container" ]
            [ "Số dư còn lại: "
                ++ ((user.balance - Maybe.withDefault 0 model.withdraw_amount)
                        |> toFloat
                        |> format { usLocale | decimals = 0, thousandSeparator = "." }
                        |> append_balance " (VND)"
                   )
                |> text
            ]
        , div [ class "input-container" ]
            [ input
                [ attribute "amount" "required"
                , attribute "type" "number"
                , attribute "placeholder" "Số tiền"

                -- , Html.Attributes.value <| String.fromInt <| Maybe.withDefault 0 <| model.withdraw_amount
                , autocomplete False
                , onInput SetWithDrawAmount
                ]
                [ model.withdraw_amount |> Maybe.withDefault 0 |> String.fromInt |> text ]
            , div [ class "bar" ]
                []
            ]
        , div [ class "button-container" ]
            [ button
                [ if String.length model.select_bank_withdraw < 1 then
                    onClick FormValid
                    --     else if  model.withdraw_amount == Nothing then
                    --         onClick FormValid

                  else
                    onClick (WithDrawCmd cred)
                ]
                [ span []
                    [ text "Rút tiền" ]
                ]
            ]
        ]


selectview : User -> Html Msg
selectview user =
    -- only bank actived can use
    case List.head <| [ Maybe.withDefault <| List.filter (\x -> String.length x.is_activated > 0) <| user.banks ] of
        Nothing ->
            div
                [ class "form-control"
                , id "selectbank"
                ]
                [ text "Vui lòng thêm ngân hàng." ]

        Just banks ->
            select
                [ class "form-control"
                , id "selectbank"
                , onInput SetSelectedBank
                , Html.Attributes.autofocus True
                ]
                [ option [] [ text "Chọn" ]
                , optgroup [ Html.Attributes.attribute "label" "Danh sách ngân hàng đã được xác thực" ] <|
                    bankview user
                ]


bankview : User -> List (Html Msg)
bankview user =
    List.map optionview <|
        List.filter (\x -> String.length x.is_activated > 0) <|
            user.banks


optionview : Bank -> Html Msg
optionview bank =
    option
        [ Html.Attributes.value bank.domaintagbank
        ]
        [ text <| bank.name ]


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
