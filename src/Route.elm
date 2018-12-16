module Route exposing (Route(..), fromUrl, href, parser, pushUrl, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- ROUTING


type Route
    = Home
    | Root
    | Login
    | AddCard
    | Profile
    | About


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top

        -- , Parser.map Account (s "account")
        -- , Parser.map About (s "about")
        , Parser.map Login (s "account")
        , Parser.map AddCard (s "addcard")
        , Parser.map Profile (s "profile")
        , Parser.map About (s "about")
        ]



-- PUBLIC HELPERS


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    --
    -- The Elm SPA Examples uses `#` for routings, to do so they construct the url like this:
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- url
--     |> Parser.parse parser
-- INTERNAL HELPERS


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                About ->
                    [ "about" ]

                Home ->
                    []

                Root ->
                    []

                -- Account ->
                --     [ "account" ]
                AddCard ->
                    [ "addcard" ]

                -- About ->
                --     [ "about" ]
                Login ->
                    [ "account" ]

                Profile ->
                    [ "profile" ]
    in
    "#" ++ String.join "/" pieces
