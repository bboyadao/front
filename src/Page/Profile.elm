module Page.Profile exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (..)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Bootstrap.CDN as CDN
import Bootstrap.Tab as Tab exposing (..)
import Bootstrap.Table as Table
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (spanishLocale, usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, list, string)
import Json.Decode.Pipeline exposing (hardcoded, required, requiredAt)
import Json.Encode exposing (encode, string)
import Session exposing (Session)
import Username exposing (Username)


subscriptions : Model -> Sub Msg
subscriptions model =
    Tab.subscriptions model.tabState TabMsg


toSession : Model -> Session
toSession model =
    model.session


type alias Model =
    { session : Session
    , user : User
    , tabState : Tab.State
    , errorMsg : String
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
      , tabState = Tab.initialState
      , errorMsg = ""
      }
    , Api.get Endpoint.user (Session.cred session) (Decode.field "user" formDecoder)
        |> Http.send FetchMe
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


type Msg
    = NoOp
    | TabMsg Tab.State
    | FetchMe (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadUrl text ->
            "Bad Url: " ++ text

        Http.Timeout ->
            "Http Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus response ->
            "Invalid Cridential!" ++ Debug.toString response.body

        Http.BadPayload message response ->
            "Bad Http Payload: "
                ++ Debug.toString message
                ++ " ("
                ++ Debug.toString response.status.code
                ++ ")"



-- SUBSCRIPTIONS


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Trang cá nhân"
    , content =
        let
            title =
                "Trang cá nhân"

            user =
                model.user
        in
        main_
            [ class "mdl-layout__content"
            ]
            [ stylesheet
            , div [ class "page-content" ]
                [ div [ class "card" ] []
                , div [ class "card" ]
                    [ h1 [ class "title" ] [ text title ]
                    , tab model
                    ]
                ]
            ]
    }


tab : Model -> Html Msg
tab model =
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
                                [ button []
                                    [ span []
                                        [ text "Thông tin" ]
                                    ]
                                ]
                            ]
                    , pane = Tab.pane [] [ hr [] [], info_tab model.user ]
                    }
                , Tab.item
                    { id = "trans"
                    , link =
                        Tab.link [ class "button-container" ]
                            [ div []
                                [ button []
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


info_tab : User -> Html msg
info_tab user =
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
                [ listbankUser user.banks
                ]
            ]
        ]


append_balance : String -> String -> String
append_balance str str1 =
    str1 ++ str


listbankUser : List Bank -> Html msg
listbankUser banks =
    ul []
        [ li [ class "list-bank" ]
            [ aside
                [ class "bank-card" ]
                [ strong [ class "" ]
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
