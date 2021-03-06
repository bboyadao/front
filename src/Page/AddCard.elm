port module Page.AddCard exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api exposing (Cred)
import Api.Endpoint as Endpoint exposing (Endpoint(..), card_final, config_addcard, final, tran, url_transcard)
import Bootstrap.Button as Button exposing (onClick)
import Bootstrap.Modal as Modal exposing (..)
import Browser.Navigation as Nav
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (spanishLocale, usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Html.Events.Extra exposing (onChange)
import Http exposing (..)
import HttpBuilder exposing (..)
import Json.Decode as Decode exposing (Decoder, field, map, map3, string)
import Json.Decode.Pipeline as JPipeline exposing (hardcoded, optional, required, requiredAt)
import Json.Encode as Encode exposing (..)
import List.Extra exposing (getAt)
import Route exposing (Route)
import Session exposing (Session)
import Utils exposing (httpErrorString)


port exchangetoken : (String -> msg) -> Sub msg



-- MODEL


type Message
    = Success
    | Error


type alias Card =
    { name : String
    , code : String
    , values : List String
    }


type alias Model =
    { session : Session
    , token : String
    , message : String
    , arrayvalue : List String
    , arraycardtype : List String
    , card_value : String
    , card_type : String
    , seri : String
    , code : String
    , captcha_csrf : String
    , captcha_img : String
    , valid_in : String
    , captcha_type : String
    , captcha_txt : String
    , finalresult : String
    , errorsMsg : List String
    , successmessages : List String
    , cards : List Card
    , card : Card
    , selected_card : String
    , type_mess : Maybe Message
    , modalVisibility : Modal.Visibility
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , token = ""
      , message = "messsssssss"
      , arraycardtype = []
      , arrayvalue = []
      , card_value = ""
      , card_type = ""
      , seri = ""
      , code = ""
      , captcha_csrf = ""
      , captcha_img = ""
      , captcha_type = ""
      , captcha_txt = ""
      , valid_in = ""
      , finalresult = ""
      , errorsMsg = []
      , successmessages = []
      , cards = []
      , card = { name = "", code = "", values = [] }
      , selected_card = ""
      , type_mess = Nothing
      , modalVisibility = Modal.hidden
      }
    , Api.get Endpoint.config_addcard (Session.cred session) cardListDecoder
        |> Http.send GetConfig
    )


cardListDecoder : Decoder (List Card)
cardListDecoder =
    Decode.list cardDecoder


cardDecoder : Decoder Card
cardDecoder =
    Decode.succeed Card
        |> required "name_service" Decode.string
        |> required "mon_id" Decode.string
        |> required "value" (Decode.list Decode.string)


type Msg
    = NoOp
    | ChangeCaptcha String
    | ChangeValueMsg String
    | ChangeCardTypeMsg String
    | Value_Follow_Card_Type_Msg String
    | Update_User_Selected_Card (List String)
    | ChangeSeri String
    | ChangeCode String
    | SubmitCard Cred
    | FinalResult (Result Http.Error String)
    | GetCaptcha (Result Http.Error { img : String, url : String })
    | GotToken String
    | SubmitCaptcha Cred
    | GetConfig (Result Http.Error (List Card))
    | AnimateModal Modal.Visibility
    | CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetConfig (Err val) ->
            ( { model | errorsMsg = [ httpErrorString val ] }, Cmd.none )

        -- GetConfig (Ok val) ->
        --     ( model, Cmd.none )
        GetConfig (Ok val) ->
            let
                cards =
                    model.card

                mapcard card =
                    { card
                        | name = card.name
                        , code = card.code
                        , values = card.values
                    }

                newcard =
                    List.map mapcard val

                -- result =
                --     { model | card = newcard }
            in
            ( { model | cards = newcard }, Cmd.none )

        -- ( result |> Debug.log "new", Cmd.none )
        ChangeCaptcha capt ->
            ( { model | captcha_txt = capt }, Cmd.none )

        SubmitCaptcha cred ->
            ( model, submitcaptcha model cred )

        GotToken sth ->
            ( { model | token = sth }, Cmd.none )

        -- help me more
        FinalResult result ->
            ( { model | valid_in = "", captcha_txt = "" }, Cmd.none )

        GetCaptcha result ->
            getCaptchaCompleted model result

        SubmitCard cred ->
            ( model, submitCardCmd model cred )

        ChangeCardTypeMsg str ->
            ( { model | card_type = str }, Cmd.none )

        Value_Follow_Card_Type_Msg value ->
            find_card_by_name value model

        ChangeValueMsg str ->
            ( { model | card_value = str }, Cmd.none )

        Update_User_Selected_Card values ->
            ( { model | arrayvalue = values }, Cmd.none )

        ChangeSeri str ->
            ( { model | seri = str }, Cmd.none )

        ChangeCode str ->
            ( { model | code = str }, Cmd.none )

        AnimateModal visibility ->
            ( { model | modalVisibility = visibility }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden, errorsMsg = [] }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


submitCardCmd : Model -> Cred -> Cmd Msg
submitCardCmd model cred =
    let
        bod =
            Encode.object
                [ ( "card_type", Encode.string model.card_type )
                , ( "value", Encode.string model.card_value )
                , ( "serial", Encode.string model.seri )
                , ( "code", Encode.string model.code )
                ]
                |> Http.jsonBody
    in
    decodeResultCaptcha
        |> Api.post Endpoint.tran (Just cred) bod
        |> Http.send GetCaptcha


submitcaptcha : Model -> Cred -> Cmd Msg
submitcaptcha model cred =
    let
        bod =
            Encode.object
                [ ( "captcha_txt", Encode.string model.captcha_csrf )
                ]
                |> Http.jsonBody
    in
    decodeResultCaptchaFinal
        |> Api.post (Endpoint.card_final model.valid_in) (Just cred) bod
        |> Http.send FinalResult


decodeResultCaptcha : Decoder { img : String, url : String }
decodeResultCaptcha =
    Decode.map2 (\img url -> { img = img, url = url })
        (Decode.at [ "captcha", "captcha_img" ] Decode.string)
        (Decode.at [ "valid_in" ] Decode.string)


encodecaptchafinal : Model -> Encode.Value
encodecaptchafinal model =
    Encode.object
        [ ( "captcha_txt", Encode.string model.captcha_csrf )
        ]


decodeResultCaptchaFinal : Decoder String
decodeResultCaptchaFinal =
    Decode.at [ "finalresult" ] Decode.string


getCaptchaCompleted : Model -> Result Http.Error { img : String, url : String } -> ( Model, Cmd Msg )
getCaptchaCompleted model result =
    case result of
        Ok sometext ->
            ( { model
                | captcha_img = sometext.img
                , valid_in = sometext.url
                , successmessages = [ "Vui lòng nhập mã bảo mật." ]
                , errorsMsg = []
                , type_mess = Just Success
                , modalVisibility = Modal.shown
              }
              --  |> Debug.log "got you"
            , Cmd.none
            )

        Err error ->
            ( { model
                | errorsMsg = [ "" ]
                , successmessages = []
                , type_mess = Just Error
                , modalVisibility = Modal.shown
              }
            , Cmd.none
            )


encodecard : Model -> Encode.Value
encodecard model =
    Encode.object
        [ ( "card_type", Encode.string model.card_type )
        , ( "value", Encode.string model.card_value )
        , ( "serial", Encode.string model.seri )
        , ( "code", Encode.string model.code )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ exchangetoken GotToken
        , Modal.subscriptions model.modalVisibility AnimateModal
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Nạp Thẻ"
    , content =
        if String.length model.captcha_img > 0 then
            case Session.cred model.session of
                Just cred ->
                    someview model cred

                Nothing ->
                    div [ class "container" ] [ div [ class "jumbotron mt-5" ] [ text "Bạn cần đăng nhập để thực hiện chức năng này" ] ]

        else
            case Session.cred model.session of
                Just cred ->
                    content_view model cred

                Nothing ->
                    div [ class "container" ] [ div [ class "jumbotron mt-5" ] [ text "Bạn cần đăng nhập để thực hiện chức năng này" ] ]
    }


content_view : Model -> Cred -> Html Msg
content_view model cred =
    main_ [ class "mdl-layout__content" ]
        [ div [ class "page-content" ]
            [ div [ class "card" ] []
            , div [ class "card" ]
                [ Html.h1 [ class "title" ]
                    [ text "Nạp Thẻ" ]
                , Html.h4 [] [ text model.token ]
                , div [ class "wrap" ]
                    [ div [ class "input-container" ]
                        -- https://ellie-app.com/kcF6mQRvNQa1
                        [ select
                            [ id "card-name"
                            , class "myselect"
                            , onChange Value_Follow_Card_Type_Msg
                            ]
                            [ option [] [ text "Chọn loại thẻ" ]
                            , optgroup [] <| List.map cardnameview model.cards
                            ]
                        ]
                    , div [ class "input-container" ]
                        [ select
                            [ id "card_type"
                            , class "myselect"
                            , onInput ChangeValueMsg
                            ]
                            [ option [] [ text "Chọn mệnh giá" ]
                            , optgroup [] <| List.map viewOptionvalue model.arrayvalue
                            ]
                        ]
                    , div [ class "input-container" ]
                        [ input
                            [ id "seri"
                            , attribute "required" "required"
                            , autocomplete False
                            , onInput ChangeSeri
                            ]
                            []
                        , label [ for "seri" ]
                            [ text "Sê-Ri" ]
                        , div [ class "bar" ]
                            []
                        ]
                    , div [ class "input-container" ]
                        [ input
                            [ id "code"
                            , attribute "required" "required"
                            , autocomplete False
                            , onInput ChangeCode
                            ]
                            []
                        , label [ for "code" ]
                            [ text "Mã Thẻ" ]
                        , div [ class "bar" ]
                            []
                        ]
                    , div [ class "button-container" ]
                        [ Html.button
                            [ onClick (SubmitCard cred)
                            ]
                            [ span []
                                [ text "Nạp Thẻ" ]
                            ]
                        ]
                    , div [ class "footer" ] []
                    ]
                , stylesheet
                , modalView model
                ]
            ]
        ]


cardnameview : Card -> Html Msg
cardnameview card =
    option
        [ -- Html.Attributes.selected True
          Html.Attributes.value card.name

        --   , Html.Attributes.selected True
        ]
        [ card.name |> text ]


viewOptionvalue : String -> Html Msg
viewOptionvalue op =
    option
        [ Html.Attributes.value op
        , Html.Attributes.selected False
        ]
        [ op
            |> String.toFloat
            |> Maybe.withDefault 0
            |> format { usLocale | decimals = 0, thousandSeparator = "." }
            |> text
        ]


updateArrayValues : Card -> Model -> ( Model, Cmd Msg )
updateArrayValues card model =
    ( { model | arrayvalue = card.values }, Cmd.none )


showNestValues : String -> Html Msg
showNestValues value =
    option [ Html.Attributes.value value ]
        [ value
            |> String.toFloat
            |> Maybe.withDefault 0
            |> format { usLocale | decimals = 0, thousandSeparator = "." }
            |> text
        ]


supportCardName : Card -> Html Msg
supportCardName card =
    option
        [ Html.Attributes.value card.name ]
        [ card.name
            |> text
        ]


someview : Model -> Cred -> Html Msg
someview model cred =
    main_ [ class "mdl-layout__content" ]
        [ div [ class "page-content" ]
            [ div [ class "card" ] []
            , div [ class "card" ]
                [ Html.h1 [ class "title" ]
                    [ text "Vui Lòng Nhập Mã Bảo Mật" ]
                , div []
                    [ img
                        [ class "img_captcha"
                        , src model.captcha_img
                        ]
                        []
                    , div [ class "input-container" ]
                        [ input
                            [ id "seri"
                            , attribute "required" "required"
                            , autocomplete False
                            , onInput ChangeCaptcha
                            ]
                            []
                        , label [ for "seri" ]
                            [ text "Mã Bảo Mật" ]
                        , div [ class "bar" ]
                            []
                        ]
                    , div [ class "button-container" ]
                        [ button
                            [ onClick (SubmitCaptcha cred)
                            ]
                            [ span []
                                [ text "Xác Nhận" ]
                            ]
                        ]
                    , div [ class "footer" ] []
                    ]
                , stylesheet
                , modalView model
                ]
            ]
        ]


viewOptionCardType : String -> String -> Html Msg
viewOptionCardType selectedString op =
    option [ Html.Attributes.value op ]
        [ op
            |> text
        ]


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


find_card_by_name : String -> Model -> ( Model, Cmd Msg )
find_card_by_name cardname model =
    let
        findcard =
            List.head (List.filter (\m -> m.name == cardname) model.cards)
    in
    case findcard of
        Just card ->
            ( { model | arrayvalue = card.values, card_type = card.name }, Cmd.none )

        _ ->
            ( model, Cmd.none )


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
                |> Modal.header [ class "modal-header badge badge-danger" ] [ Html.h5 [] [ text "Lỗi" ] ]
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
                |> Modal.header [ class "modal-header badge badge-success" ] [ Html.h5 [] [ text "Thành Công" ] ]
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
