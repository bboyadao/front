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
    | MYpage
    -- | AboutUs
    



-- VIEW


view :  Page -> { title : String, content : Html msg } -> Document msg
view  page { title, content } =
    { title = title ++ " - Elements"
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
                    [ text "Home" ]
                -- , a [ class "mdl-navigation__link", Route.href Route.About ]
                --     [ text "About" ]
                , a [ class "mdl-navigation__link", Route.href Route.Login ]
                    [ text "Account" ]
                , a [ class "mdl-navigation__link", Route.href Route.Mpage ]
                    [ text "Mpage" ]
                ]
            ]
        ]
    
viewFooter : Html msg
viewFooter =
    footer
        [ class "container-fluid" ]
            [ 
                div [ class "container" ]
                    [ text "This is the footer." ]
        ]