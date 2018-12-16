module Page exposing (Page(..), view, viewFooter, viewHeader)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Login exposing (Model)
import Route exposing (Route)
import Username exposing (..)
import Viewer exposing (Viewer)



-- PAGE TYPES


type Page
    = Other
    | Login
    | Home
    | AddCard
    | Profile



-- | AboutUs
-- isActive : Page -> Route -> Bool
-- isActive page route =
--     case ( page, route ) of
--         ( Home, Route.Home ) ->
--             True
--         ( Login, Route.Login ) ->
--             True
--         ( Register, Route.Register ) ->
--             True
--         ( Settings, Route.Settings ) ->
--             True
--         ( Profile pageUsername, Route.Profile routeUsername ) ->
--             pageUsername == routeUsername
--         ( NewArticle, Route.NewArticle ) ->
--             True
--         _ ->
--             False
-- VIEW


view : Maybe Viewer -> Page -> { title : String, content : Html msg } -> Document msg
view maybeviewer page { title, content } =
    { title = title ++ " - Mua Bán Thẻ Cào"
    , body = viewHeader page maybeviewer :: content :: [ viewFooter ]
    }


navbar : Page -> Route -> List (Html msg) -> Html msg
navbar page route linkContent =
    div []
        [ -- a [ class "mdl-navigation__link", Route.href route] linkContent]
          a
            [ classList
                [ ( "mdl-navigation__link", True )
                , ( "active", isActive page route )
                ]
            , Route.href route
            ]
            linkContent
        ]


viewMenu : Page -> Maybe Viewer -> List (Html msg)
viewMenu page maybeviewer =
    let
        linkTo =
            navbar page
    in
    case maybeviewer of
        Just viewer ->
            let
                username =
                    Viewer.username viewer

                avatar =
                    Viewer.avatar viewer
            in
            [ navbar page Route.About [ text "Giới thiệu" ]

            -- , navbar page Route.Root [ text "Hướng Dẫn" ]
            , navbar page Route.AddCard [ text "Nạp Thẻ" ]
            , linkTo Route.Profile [ Username.toHtml username ]
            ]

        Nothing ->
            [ navbar page Route.About [ text "Giới thiệu" ]

            -- , navbar page Route.Root [ text "Hướng Dẫn" ]
            , navbar page Route.AddCard [ text "Nạp Thẻ" ]
            , navbar page Route.Login [ text "Tài khoản" ]
            ]


viewHeader : Page -> Maybe Viewer -> Html msg
viewHeader page maybeviewer =
    header [ class "mdl-layout__header mdl-layout__header--transparent" ]
        [ div [ class "mdl-layout__header-row" ]
            [ span [ class "mdl-layout-title" ]
                [ navbar page Route.Home [ text "Home" ] ]
            , div [ class "mdl-layout-spacer" ] []
            , nav [ class "mdl-navigation mdl-layout--large-screen-only" ] <|
                viewMenu page maybeviewer

            -- nav
            ]
        ]


viewFooter : Html msg
viewFooter =
    footer
        [ class "container-fluid" ]
        [ div [ class "container" ]
            [ text "Beta V0.1." ]
        ]


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Login, Route.Login ) ->
            True

        ( Profile, Route.Profile ) ->
            True

        ( Home, Route.Root ) ->
            True

        ( AddCard, Route.AddCard ) ->
            True

        _ ->
            False
