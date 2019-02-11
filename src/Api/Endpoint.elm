module Api.Endpoint exposing
    ( Endpoint
    , addBank_url
    , articles
    , card_final
    , cashout_url
    , config_addcard
    , feed
    , final
    , gotoMainsite
    , login
    , login_url
    , myRedirect
    , profiles
    , request
    , root_url
    , signup_url
    , tags
    , tran
    , url_transcard
    , user
    , users
    )

import Browser.Navigation as Nav exposing (load)
import Http
import Url.Builder exposing (QueryParameter)
import Username exposing (Username)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , withCredentials : Bool
    }
    -> Http.Request a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , withCredentials = config.withCredentials
        }



-- TYPES


{-| Get a URL to the Conduit API.
This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.
-}
type Endpoint
    = Endpoint String


root_url : String
root_url =
    -- "http://sec.localhost:8000"
    -- "http://sec.localhost:8000"
    -- "https://adaostore.herokuapp.com"
    "https://sec.chuthe.com"


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    Url.Builder.crossOrigin root_url
        ("api/v1" :: paths)
        queryParams
        |> Endpoint


gotoMainsite : Cmd msg
gotoMainsite =
    load root_url



-- Cmd redirect


myRedirect : Endpoint -> Cmd msg
myRedirect redirect_url =
    load (redirect_url |> unwrap)



-- ENDPOINTS


cashout_url : Endpoint
cashout_url =
    url [ "tran", "cashout/" ] []


addBank_url : Endpoint
addBank_url =
    url [ "account", "addbank/" ] []


config_addcard : Endpoint
config_addcard =
    url [ "tran", "config/" ] []


signup_url : Endpoint
signup_url =
    url [ "account", "signup/" ] []


login_url : Endpoint
login_url =
    url [ "account", "login/" ] []


tran : Endpoint
tran =
    url [ "tran", "addcard/" ] []


card_final : String -> Endpoint
card_final valid_id =
    url [ "tran", "final", valid_id ++ "/" ] []


login : Endpoint
login =
    url [ "users", "login/" ] []


user : Endpoint
user =
    url [ "account/" ] []


users : Endpoint
users =
    url [ "users/" ] []



-- ARTICLE ENDPOINTS
-- article : Slug -> Endpoint
-- article slug =
--     url [ "articles", Slug.toString slug ] []
-- comments : Slug -> Endpoint
-- comments slug =
--     url [ "articles", Slug.toString slug, "comments" ] []
-- comment : Slug -> CommentId -> Endpoint
-- comment slug commentId =
--     url [ "articles", Slug.toString slug, "comments", CommentId.toString commentId ] []
-- favorite : Slug -> Endpoint
-- favorite slug =
--     url [ "articles", Slug.toString slug, "favorite" ] []


articles : List QueryParameter -> Endpoint
articles params =
    url [ "articles" ] params


profiles : Username -> Endpoint
profiles uname =
    url [ "profiles", Username.toString uname ] []


feed : List QueryParameter -> Endpoint
feed params =
    url [ "articles", "feed" ] params


tags : Endpoint
tags =
    url [ "tags" ] []


url_transcard : String
url_transcard =
    root_url ++ "trans/"


final : String
final =
    root_url ++ "final/"
