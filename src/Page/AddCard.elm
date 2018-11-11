port module Page.AddCard exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Session exposing (Session)
import Json.Decode.Pipeline as JPipeline exposing (required, optional, hardcoded,requiredAt)
import Json.Encode as Encode exposing(..)
import Json.Decode as Decode exposing(Decoder,field,map3,string)
import Http exposing(..)
import Config exposing (final,url_transcard)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (spanishLocale, usLocale)
import Utils exposing (httpErrorString)
import HttpBuilder exposing (..)
port exchangetoken : (String -> msg) -> Sub msg

-- MODEL



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
    , captcha_img: String
    , valid_in : String
    , captcha_type: String
    , captcha_txt : String
    , finalresult : String
    , errorMsg : String
    }


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , token = ""
      , message = "MFFFfffffff"
      , arrayvalue= ["10000","20000","50000","100000","1000000","10000000"]
      , arraycardtype = ["Viettel","Vina","50000","100000","1000000","10000000"]
      , card_value = "10000"
      , card_type= "Viettel"
      , seri =""
      , code =""
      , captcha_csrf = ""
      , captcha_img = ""
      , captcha_type = ""
      , captcha_txt = ""
      , valid_in = ""
      , finalresult = ""
      , errorMsg = ""
      }
    , Cmd.none
    )







-- UPDATE


type Msg
    = NoOp
    | ChangeCaptcha String
    | ChangeValueMsg String
    | ChangeCardTypeMsg String
    | ChangeSeri String
    | ChangeCode String
    | SubmitCard
    | FinalResult (Result Http.Error String)
    | GetCaptcha (Result Http.Error { img : String, url : String })
    | GotToken String
    | SubmitCaptcha

submitCardCmd : Model  -> Cmd Msg
submitCardCmd model =
    HttpBuilder.post url_transcard
        |> withHeaders [
                ("Authorization", "Token "++ model.token)
              , ("Accept", "application/json")
                ]
        |> withJsonBody (encodecard model)
        |> withTimeout 10000
        |> withExpectJson decodeResultCaptcha
        |> withCredentials
        |> HttpBuilder.send GetCaptcha

decodeResultCaptcha : Decoder {img:String,url:String}
decodeResultCaptcha =
    Decode.map2(\img url -> {img= img, url = url})
        (Decode.at ["captcha","captcha_img"] string)
        (Decode.at ["valid_in"] string)

encodecaptchafinal : Model -> Encode.Value
encodecaptchafinal model =
    Encode.object
        [ ("captcha_txt", Encode.string model.captcha_csrf)
        ]

submitcaptcha : Model  -> Cmd Msg 
submitcaptcha model =
    HttpBuilder.post (final ++ model.valid_in++"/")
        |> withHeaders [
                ("Authorization", "Token "++ model.token)
              , ("Accept", "application/json")
                ]
        |> withJsonBody (encodecaptchafinal model)
        |> withTimeout 10000
        |> withExpectJson decodeResultCaptchaFinal
        |> withCredentials
        |> HttpBuilder.send FinalResult



decodeResultCaptchaFinal : Decoder String
decodeResultCaptchaFinal =
        Decode.at ["finalresult"] string



getCaptchaCompleted : Model -> Result Http.Error {img:String,url:String} ->   ( Model, Cmd Msg )
getCaptchaCompleted model result =
    case result of
        Ok sometext ->
            (
                {model | captcha_img = sometext.img,valid_in=sometext.url}|> Debug.log("got you"),Cmd.none)


        Err error ->
            ( { model | errorMsg =  httpErrorString error }, Cmd.none )




encodecard : Model -> Encode.Value
encodecard model =
    Encode.object
        [ ("card_type", Encode.string model.card_type)
        , ("value", Encode.string model.card_value)
        , ("serial", Encode.string model.seri)
        , ("code", Encode.string model.code)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeCaptcha capt -> 
            ({model | captcha_txt= capt},Cmd.none)
        SubmitCaptcha ->
            (model, submitcaptcha model )
        GotToken sth->
            ({model|token=sth},Cmd.none)
        -- help me more
        FinalResult result->
            ({model| valid_in="",captcha_txt=""},Cmd.none)
        
        GetCaptcha result->
            getCaptchaCompleted model result

        SubmitCard ->
            ( model, submitCardCmd model )

        ChangeCardTypeMsg str ->
            ( {model|card_type =  str}, Cmd.none)

        ChangeValueMsg str ->
            ( {model|card_value =  str}, Cmd.none)
        ChangeSeri str ->
            ({model | seri = str},Cmd.none)
        ChangeCode str ->
            ({model |code = str},Cmd.none)

        NoOp ->
            (model,Cmd.none)
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    exchangetoken GotToken



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session






-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Add Card"
    , content = if String.length model.captcha_img >0 then someview model
    else
    content_view model

    }
someview :  Model -> Html Msg
someview model=
    main_ [ class "mdl-layout__content" ][
        div [ class "page-content" ][
                div [ class "None" ][]


                ,div [ class "card" ] [
                    h1 [ class "title" ]
                    [ text "Vui Lòng Nhập Mã Bảo Mật" ]
                    ,div [][
                         img [class "img_captcha"
                         ,src model.captcha_img] []
                        , div [ class "input-container"]



            [ input [ id "seri"

            ,attribute  "required" "required"
            , autocomplete False
            ,onInput ChangeCaptcha
            ]
                []
            , label [ for "seri" ]
                [ text "Mã Bảo Mật" ]
            , div [ class "bar" ]
                []
        ]


    , div [ class "button-container" ]
        [ button [
            onClick SubmitCaptcha
                ]
            [ span []
                [ text "Xác Nhận" ]
            ]
        ]
    , div [ class "footer" ]
        [
        ]
    ]
    ,stylesheet]]]


viewOptionvalue :  String -> String -> Html Msg
viewOptionvalue selectedString op =

    option [  Html.Attributes.value op  ] [ op
            |> String.toFloat
            |> Maybe.withDefault  0
            |> format { usLocale| decimals = 0,thousandSeparator = "."}
            |>text  ]

viewOptionCardType :  String -> String -> Html Msg
viewOptionCardType selectedString op =

    option [  Html.Attributes.value op  ] [ op

            |>text  ]



content_view : Model -> Html Msg
content_view model =

    main_ [ class "mdl-layout__content" ]
                [ div [ class "page-content" ]

    [
    div [ class "None" ][]


    ,div [ class "card" ]
        [
            h1 [ class "title" ]
            [ text "Nạp Thẻ" ]
        -- ,h4[] [text  model.token ]



    , div []
        [   div [ class "input-container"]
        [    select [id "value",
                        class  "myselect" ,
                        onInput  ChangeValueMsg  ]
            (List.map
                (viewOptionCardType  model.card_type)
                model.arraycardtype
            )
        ]
        ,   div [ class "input-container"]
        [  select [id "card_type",
                        class  "myselect" ,
                        onInput  ChangeCardTypeMsg  ]
            (List.map
                (viewOptionvalue  model.card_value)
                model.arrayvalue

            )



            -- https://ellie-app.com/kcF6mQRvNQa1
        ]


        ,div [ class "input-container" ]
            [ input [ id "seri"

            ,attribute  "required" "required"
            , autocomplete False
            ,onInput ChangeSeri
            ]
                []
            , label [ for "seri" ]
                [ text "Sê-Ri" ]
            , div [ class "bar" ]
                []
        ]

    , div [ class "input-container" ]
        [ input [ id "code"
        ,attribute "required" "required"
        , autocomplete False
        ,onInput ChangeCode]
            []
        , label [ for "code" ]
            [ text "Mã Thẻ" ]
        , div [ class "bar" ]
            []
        ]

    , div [ class "button-container" ]
        [ button [
            onClick SubmitCard
                ]
            [ span []
                [ text "Nạp Thẻ" ]
            ]
        ]
    , div [ class "footer" ]
        [
        ]
    ]

    ,stylesheet
                    ]
                        ]
                            ]







stylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "static/css/Account/login.css"

            ]
        children = []
    in
        node tag attrs children
