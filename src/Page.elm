module Page exposing (Page(..), view, viewFooter, viewHeader)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Page.Login exposing (Model)


-- PAGE TYPES


type Page
    = Other
    | Login
    | Home
    | AddCard
    -- | AboutUs
    



-- VIEW


view :  Page -> { title : String, content : Html msg } -> Document msg
view  page { title, content } =
    { title = title ++ " - Mua Bán Thẻ Cào"
    , body = 
        
        viewHeader  :: content  :: [  viewFooter ]
    }


viewHeader :  Html msg
viewHeader   =
    header [ class "mdl-layout__header mdl-layout__header--transparent" ]
            [ 
            div [ class "mdl-layout__header-row" ]
                [ 
                span [ class "mdl-layout-title" ]
                    [ text "Title" ]
            , div [ class "mdl-layout-spacer" ]
                    []
            , nav [ class "mdl-navigation mdl-layout--large-screen-only" ]
                [ 
                a [ class "mdl-navigation__link", Route.href Route.Home]
                    [ text "Trang Chủ" ]
                -- , a [ class "mdl-navigation__link", Route.href Route.About ]
                --     [ text "About" ]
                , a [ class "mdl-navigation__link", Route.href Route.Login ]
                    [ text "Tài Khoản" ]
                , a [ class "mdl-navigation__link", Route.href Route.AddCard ]
                    [ text "Nạp Thẻ" ]
                , a [ class "mdl-navigation__link" ]
                    [ text "Rút Tiền" ]
                , a [ class "mdl-navigation__link"]
                    [ text "Hướng Dẫn" ]
                ]
            ]
        ]
    
viewFooter : Html msg
viewFooter =
    footer
        [ class "container-fluid" ]
            [ 
                div [ class "container" ]
                    [ text "Beta V0.1." ]
        ]