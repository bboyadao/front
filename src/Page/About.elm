-- module Page.Profile exposing (Model, Msg, init, subscriptions, toSession, update, view)


module Page.About exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Bootstrap.Button as Button exposing (..)
import Bootstrap.Grid as Grid exposing (..)
import Bootstrap.Grid.Col as Col exposing (..)
import Bootstrap.Modal as Modal exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Session exposing (Session)
import Username exposing (Username)


type alias Model =
    { session : Session
    , modalVisibility : Modal.Visibility

    -- , timeZone : Time.Zone
    , errors : List String

    -- , feedTab : FeedTab
    , feedPage : String

    -- Loaded independently from server
    -- , author : Status Author
    -- , feed : Status Feed.Model
    }


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
      , modalVisibility = Modal.shown
      }
    , Cmd.none
    )


toSession : Model -> Session
toSession model =
    model.session


type Msg
    = NoOp
    | CloseModal
    | CC
    | AnimateModal Modal.Visibility



-- VIEW


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }
            , Cmd.none
            )

        CC ->
            ( { model | modalVisibility = Modal.shown }
            , Cmd.none
            )

        AnimateModal visibility ->
            ( { model | modalVisibility = visibility }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Modal.subscriptions model.modalVisibility AnimateModal ]



-- EXPORT


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        div [] <| [ modalview model ]

    -- div [ class "container-fluid" ]
    --     [ div [ class "container" ]
    --         [ div [ class "jumbotron mt-5" ]
    --             [ text model.feedPage
    --             , br [] []
    --             , a
    --                 [-- Route.href Route.About
    --                 ]
    --                 [ text "Go to the Login Page" ]
    --             ]
    --         ]
    --     ]
    }


modalview : Model -> Html Msg
modalview model =
    Grid.container []
        [ --     Button.button
          --     [ onClick CC ]
          --     [ text "Show modal" ]
          Modal.config CloseModal
            |> Modal.h5 [] [ text "Modal header" ]
            |> Modal.body [] [ text "Modal body" ]
            |> Modal.footer []
                [ Button.button
                    [ Button.outlinePrimary, onClick <| AnimateModal Modal.hiddenAnimated ]
                    [ text "Close" ]
                ]
            |> Modal.view model.modalVisibility
        ]



-- UPDATE
