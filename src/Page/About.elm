-- module Page.Profile exposing (Model, Msg, init, subscriptions, toSession, update, view)


module Page.About exposing (Model, Msg, init, toSession, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Session exposing (Session)
import Username exposing (Username)


type alias Model =
    { session : Session

    -- , timeZone : Time.Zone
    , errors : List String

    -- , feedTab : FeedTab
    , feedPage : String

    -- Loaded independently from server
    -- , author : Status Author
    -- , feed : Status Feed.Model
    }


type Msg
    = NoOp



-- VIEW


view : Model -> { title : String, content : Html msg }
view model =
    { title = "Home"
    , content =
        div [ class "container-fluid" ]
            [ div [ class "container" ]
                [ div [ class "jumbotron mt-5" ]
                    [ text model.feedPage
                    , br [] []
                    , a
                        [-- Route.href Route.About
                        ]
                        [ text "Go to the Login Page" ]
                    ]
                ]
            ]
    }



-- UPDATE


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


init : Session -> ( Model, Cmd Msg )
init session =
    let
        maybeCred =
            Session.cred session
    in
    ( { session = session

      --   , timeZone = Time.utc
      , errors = []

      --   , feedTab = defaultFeedTab
      , feedPage = "333"

      --   , author = Loading username
      --   , feed = Loading username
      }
    , Cmd.none
    )


toSession : Model -> Session
toSession model =
    model.session
