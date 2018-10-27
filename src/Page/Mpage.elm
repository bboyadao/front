module Page.Mpage exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

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
      , message = "MFFFfffffff"
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html msg }
view model =
    { title = "About Us"
    , content = content_view model
        
    }


content_view : Model -> Html msg 
content_view model =
    main_ [ class "mdl-layout__content" ]
                [ div [ class "page-content" ]
                    [ div [ class "" ]
                        [ text model.message
                        , br [] []
                        , a [ Route.href Route.Home ] [ text "Go to the Home Page" ]
                        ]
                    ]
                ]


-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session