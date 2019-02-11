module Page.Home exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , message : String
    }


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , message = "Welcome to the Home Page!"
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html msg }
view model =
    { title = "Home"
    , content =
        div [ class "container-fluid" ]
            [ section []
                [ div [ class "row mysection" ]
                    [ div [ class "leftsection col-6 col-md-4" ]
                        [ h1 [ class "myheadtitle" ] [ text "ChuThe.Com" ]
                        ]
                    , div [ class "rightsection col-12 col-md-8" ]
                        [ h4 [ class "myheadtitle" ] [ text "Chào Mừng Bạn Đến với Chuthe.com" ]
                        ]
                    ]
                ]
            ]
    }



-- UPDATE


type Msg
    = NoOp
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
