module Main exposing (main)

import Api exposing (Cred)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Page exposing (Page)
import Page.About as About
import Page.AddCard as AddCard
import Page.Blank as Blank
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Profile as Profile
import Route exposing (Route)
import Session exposing (Session, fromViewer)
import Url
import Viewer exposing (..)


init : Maybe Viewer -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url key =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromViewer key maybeViewer))


main : Program Decode.Value Model Msg



-- main : Program String Model Msg


main =
    Api.application Viewer.decoder
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type Model
    = Home Home.Model
    | Redirect Session
    | NotFound Session
    | AddCard AddCard.Model
    | Login Login.Model
    | Profile Profile.Model
    | About About.Model



-- | AboutUs AboutUs.Model


type Msg
    = Ignored
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotSession Session
    | GotAddCardMsg AddCard.Msg
    | GotProfileMsg Profile.Msg
    | GotAboutMsg About.Msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        NotFound _ ->
            Sub.none

        Profile profile ->
            Sub.map GotProfileMsg (Profile.subscriptions profile)

        AddCard addcard ->
            Sub.map GotAddCardMsg (AddCard.subscriptions addcard)

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Login login ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        _ ->
            Sub.none


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Just Route.Root ->
            ( model, Route.pushUrl (Session.navKey session) Route.Home )

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg model

        Just Route.About ->
            About.init session
                |> updateWith About GotAboutMsg model

        Just Route.AddCard ->
            AddCard.init session
                |> updateWith AddCard GotAddCardMsg model

        Just Route.Profile ->
            Profile.init session
                |> updateWith Profile GotProfileMsg model

        Nothing ->
            ( NotFound session, Cmd.none )


toSession : Model -> Session
toSession page =
    case page of
        Profile profile ->
            Profile.toSession profile

        About about ->
            About.toSession about

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

        ( GotProfileMsg subMsg, Profile profile ) ->
            Profile.update subMsg profile
                |> updateWith Profile GotProfileMsg model

        ( GotAddCardMsg subMsg, AddCard addcard ) ->
            AddCard.update subMsg addcard
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
                    Page.view (Session.viewer (toSession model)) page config
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
            viewPage Page.Other GotLoginMsg (Login.view login)

        Profile profile ->
            viewPage Page.Profile GotProfileMsg (Profile.view profile)

        About about ->
            viewPage Page.Other GotAboutMsg (About.view about)
