port module Page.Login exposing (Model, Msg(..), subscriptions, toSession, update, view,init,pushTolocal)

import Browser
import Browser.Navigation exposing(load )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.AttributeBuilder as AB
import Session exposing (Session)
import Config exposing (root_url,url_login, url_signup,gotoMainsite)
import Html.Events exposing (onInput, onClick)
import Json.Decode as Decode exposing (Decoder, int, string, float)

import Json.Decode.Pipeline exposing (required, optional, hardcoded,requiredAt)

import Json.Encode as Encode exposing(..)


import Http exposing(..)


port pushTolocal : Maybe Value -> Cmd msg


storeCredWith : Model -> Cmd msg
storeCredWith model =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [  ( "token", Encode.string model.token ) ]
                  )
                ]
    in
    pushTolocal (Just json)

init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
        , container_active= False
        , username = ""
        , email=""
        , balance  = 0
        , is_superuser  = False
        , is_staff = False
        , is_active = False
        , token  = ""
        , password  = ""
        , passwordAgain  = ""
        , errorMsg  = ""}
      
    , Cmd.none
    )


usersignupEncoder : Model -> Encode.Value
usersignupEncoder model = 
    Encode.object 
        [ ("username", Encode.string model.username)
        , ("email", Encode.string model.email) 
        , ("password1", Encode.string model.password) 
        , ("password2", Encode.string model.passwordAgain) 
        
        ]    

userEncoder : Model -> Encode.Value
userEncoder model = 
    Encode.object 
        [ ("username", Encode.string model.username)
        , ("password", Encode.string model.password) 
        ]     

toSession : Model -> Session
toSession model =
    model.session




signupUser : Model -> String -> Http.Request String
signupUser model apiUrl =
    let
        body =
            model
                |> usersignupEncoder
                |> Http.jsonBody
    in
        Http.post apiUrl body userDecoder

authUser : Model -> String -> Http.Request String
authUser model apiUrl =
    let
        body =
            model
                |> userEncoder
                |> Http.jsonBody
    in
        Http.post apiUrl body userDecoder

userDecoder : Decoder String
userDecoder =
    Decode.field "token" Decode.string

signupCmd : Model -> String -> Cmd Msg
signupCmd model apiUrl =
    Http.send GetTokenCompleted (signupUser model apiUrl)


authUserCmd : Model  -> String -> Cmd Msg
authUserCmd model apiUrl =
    Http.send GetTokenCompleted (authUser model apiUrl)

getTokenCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case result of
        Ok some_token ->
            ( 
                { 
                    model | token = some_token                 
                }|> Debug.log "got new token"
                ,storeCredWith model
                
            )

        Err error ->

            ( { model | errorMsg =  httpErrorString error }, Cmd.none )


httpErrorString : Error -> String
httpErrorString error =
    case error of
        BadUrl text ->
            "Bad Url: " ++ text
        Timeout ->
            "Http Timeout"
        NetworkError ->
            "Network Error"
        BadStatus response -> "Invalid Cridential!"  ++ Debug.toString response.body 
            
        BadPayload message response ->
            "Bad Http Payload: "
                ++ Debug.toString message
                ++ " ("
                ++ Debug.toString response.status.code
                ++ ")"
--SUB
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- MODEL
type alias Errors =
    {   
        username : String
        ,password : String 
        ,non_field_errors : String
    }
type alias Model =
    {   
        session : Session
        , container_active : Bool
        , username : String
        , email : String
        , balance : Int
        , is_superuser : Bool
        , is_staff: Bool
        , is_active: Bool
        , token : String
        , password : String
        , passwordAgain : String
        , errorMsg : String

    }




-- UPDATE


type Msg
  = SetUsername String
  | SetPassword String
  | SetPasswordAgain String
  | SetEmail String
  | ClickLogin 
  | ClickSignup
  | GetTokenCompleted (Result Http.Error String)
  | SetToken String
  | GotoMainsite 
  | Addclass_active
  | Addclass_close
  
  
  
  
 




update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    
    
    GotoMainsite -> (model, gotoMainsite)

    ClickLogin ->
        ( {model| errorMsg = ""}, authUserCmd model url_login )
    
    ClickSignup -> 
        ( {model| errorMsg = ""}, signupCmd model url_signup )
    
    SetToken str -> (
        {model | token =str}
        ,Cmd.none)
    
    SetUsername name ->
      ({ model | username = name },Cmd.none)

    SetPassword password ->
      ({ model | password = password },Cmd.none)
    
    SetEmail email ->
        ({model | email = email },Cmd.none)

    SetPasswordAgain password ->
      ({ model | passwordAgain = password },Cmd.none)

    GetTokenCompleted result ->
            getTokenCompleted model result

    Addclass_active  -> 
        ({model | container_active = True}, Cmd.none)

    Addclass_close ->
        ({model | container_active = False}, Cmd.none)
    
    

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Trang Đăng Nhập"
    , content = login_page model
        
    }

stylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "static/css/Account/login.css"
            
            ]
        children = []
    in 
        node tag attrs children

login_page : Model -> Html Msg
login_page model =
    div [ 
        Html.Attributes.classList [
                ("container", True),
                ("active", model.container_active)                
            ]
    ]
    
    [
    div [ class "card" ][]        

    , div [ class "card" ]
        [ h1 [ class "title" ]
            [ text "Đăng Nhập" ]
        ,
        
        div [class"errors",
        if String.length model.errorMsg > 0 then 
        hidden False
        else
       hidden  True
        
         ] [ li[class "error"][ text  model.errorMsg ] ]
        ,div []
            [ div [ class "input-container" ]
                [ input [ Html.Attributes.value model.username 
                , onInput SetUsername
                , attribute  "required" "required"
                , autocomplete False]
                    []
                , label [ for "username" ]
                    [ text "Tài Khoản" ]
                , div [ class "bar" ]
                    []
                ]
                
            , div [ class "input-container" ]
                [ input [ type_ "password"
                ,onInput SetPassword
                , attribute "required" "required"
                , autocomplete False ]
                    []
                , label [ for "pass" ]
                    [ text "Mật Khẩu" ]
                , div [ class "bar" ]
                    []
                ]
            , div [ class "button-container" ]
                [ button [
                    if String.length model.username >0 && String.length model.password >0 then
                    onClick ClickLogin
                    else
                    class ""
                     ]
                    [ span []
                        [ text "Đăng Nhập" ]
                    ]
                ]
            , div [ class "footer" ]
                [ a [ href "#" ]
                    [ text "Forgot your password?" ]
                ]
            ]
        ]
    , div [ class "card alt" ]
        [ div 
            [ class "toggle", onClick Addclass_active ]
            [text ""]
        , h1 [ class "title" ]
            [ text "Đăng Ký"
            , div [ class "close", onClick Addclass_close ]
                []
            ]
        , div []
            [ div [ class "input-container" ]
                [ input [ id "username", attribute "required" "required", autocomplete False
                , onInput SetUsername]
                    []
                , label [ for "username" ]
                    [ text "Tên Tài Khoản" ]
                , div [ class "bar" ]
                    []
                ]
            ,div [ class "input-container" ]
                [ input [ id "email", attribute "required" "required", autocomplete False
                , onInput SetEmail]
                    []
                , label [ for "email" ]
                    [ text "E-mail" ]
                , div [ class "bar" ]
                    []    
                ]
            , div [ class "input-container" ]
                [ input [type_ "password", id "pass", attribute "required" "required", autocomplete False
                 ,onInput SetPassword]
                    []
                , label [ for "pass" ]
                    [ text "Mật Khẩu" ]
                , div [ class "bar" ]
                    []
                ]
            , div [ class "input-container" ]
                [ input [ type_ "password", id "repass", attribute "required" "required", autocomplete False
                ,onInput SetPasswordAgain]
                    []
                , label [ for "repass" ]
                    [ text "Nhập lại Mật Khẩu" ]
                , div [ class "bar" ]
                    []
                ]
            , div [ class "button-container" ]
                [ button [onClick ClickSignup]
                    [ span []
                        [ text "Đăng Ký" ]
                    ]
                ]
            ]
        ,stylesheet    
        
        ]
    ]
