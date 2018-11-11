module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)

import Page exposing (Page)
-- import Page.AboutUs as AboutUs
import Page.AddCard as AddCard
import Page.Blank as Blank
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Route exposing (Route)
import Session exposing (Session)
import Url




main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }

-- MODEL


type Model =  Home Home.Model
    | Redirect Session
    | NotFound Session
    | AddCard AddCard.Model
    | Login Login.Model

    -- | AboutUs AboutUs.Model


type Msg
    = Ignored
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    -- | GotAboutUsMsg AboutUs.Msg
    | GotSession Session
    | GotAddCardMsg AddCard.Msg


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        AddCard addcard ->
            Sub.map GotAddCardMsg (AddCard.subscriptions addcard)

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Login login ->
            Sub.map GotLoginMsg (Login.subscriptions login)
        _  ->
            Sub.none



init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    changeRouteTo (Route.fromUrl url) (Redirect (Session.fromViewer key))

changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            ( model, Route.pushUrl (Session.navKey session) Route.Home )

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg model


        -- Just Route.About ->
        --     AboutUs.init session
        --         |> updateWith AboutUs GotAboutUsMsg model

        Just Route.AddCard ->
            AddCard.init session
                |> updateWith AddCard GotAddCardMsg model

toSession : Model -> Session
toSession page =
    case page of
        NotFound session ->
            session

        Redirect session ->
            session

        Home home ->
            Home.toSession home
        AddCard addcard ->
            AddCard.toSession addcard
        Login login ->
            Login.toSession login

        -- AboutUs aboutUs ->
        --     AboutUs.toSession aboutUs


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg model

        -- ( GotAboutUsMsg subMsg, AboutUs aboutUs ) ->
        --     AboutUs.update subMsg aboutUs
        --         |> updateWith AboutUs GotAboutUsMsg model

        ( GotAddCardMsg  subMsg, AddCard  aboutUs) ->
            AddCard.update subMsg aboutUs
                |> updateWith AddCard GotAddCardMsg model
        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )







-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view  page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            viewPage Page.Other (\_ -> Ignored) Blank.view

        NotFound _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view

        Home home ->
            viewPage Page.Home GotHomeMsg (Home.view home)

        AddCard mpage ->
            viewPage Page.AddCard GotAddCardMsg (AddCard.view mpage)

        Login login ->
            viewPage Page.Login GotLoginMsg (Login.view login)

        -- AboutUs aboutUs ->
        --     viewPage Page.Other GotAboutUsMsg (AboutUs.view aboutUs)
