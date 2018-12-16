port module Api exposing
    ( Cred(..)
    , application
    , login    
    , storeCredWith
    , username
    , viewerChanges
    , get
    , post
    )
    
import Api.Endpoint as Endpoint exposing (Endpoint)
import Avatar exposing (Avatar)
import Browser
import Browser.Navigation as Nav exposing (load)
import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Url exposing (Url)
import Username exposing (Username)

credHeader : Cred -> Http.Header
credHeader (Cred _ str) =
    Http.header "Authorization" ("Token " ++ str)


type Cred
    = Cred Username String


username : Cred -> Username
username (Cred val _) =
    val


credDecoder : Decoder Cred
credDecoder =
    Decode.succeed Cred
        |> required "username" Username.decoder
        |> required "token" Decode.string


decode : Decoder (Cred -> viewer) -> Value -> Result Decode.Error viewer
decode decoder value =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue Decode.string value
        |> Result.andThen (\str -> Decode.decodeString (Decode.field "user" (decoderFromCred decoder)) str)


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Maybe viewer -> msg) -> Decoder (Cred -> viewer) -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder (Cred -> viewer) -> Value -> Maybe viewer
decodeFromChange viewerDecoder val =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue (storageDecoder viewerDecoder) val
        |> Result.toMaybe


storageDecoder : Decoder (Cred -> viewer) -> Decoder viewer
storageDecoder viewerDecoder =
    Decode.field "user" (decoderFromCred viewerDecoder)


decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2
        (\fromCred cred -> fromCred cred)
        decoder
        credDecoder


storeCredWith : Cred -> Avatar -> Cmd msg
storeCredWith (Cred uname token) avatar =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "token", Encode.string token )
                        , ( "username", Username.encode uname )
                        , ( "user", Username.encode uname )
                        , ( "image", Avatar.encode avatar )
                        ]
                  )
                ]
    in
    storeCache (Just json)



-- Debug.log "got new token" Cmd.none


logout : Cmd msg
logout =
    storeCache Nothing


port storeCache : Maybe Value -> Cmd msg



-- APPLICATION


application :
    Decoder (Cred -> viewer)
    ->
        { init : Maybe viewer -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Decode.Value model msg



-- -> Program Decode.Value model msg


application viewerDecoder config =
    let
        init flags url navKey =
            let
                maybeViewer =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString (storageDecoder viewerDecoder))
                        |> Result.toMaybe
            in
            config.init maybeViewer url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }


login : Http.Body -> Decoder (Cred -> a) -> Http.Request a
login body decoder =
    post Endpoint.login Nothing body (Decode.field "user" (decoderFromCred decoder))


post : Endpoint -> Maybe Cred -> Body -> Decoder a -> Http.Request a
post url maybeCred body decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson decoder
        , headers =
            case maybeCred of
                Just cred ->
                    [ credHeader cred ]

                Nothing ->
                    []
        , body = body
        , timeout = Nothing
        , withCredentials = False
        }








get : Endpoint -> Maybe Cred -> Decoder a -> Http.Request a
get url maybeCred decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson decoder
        , headers =
            case maybeCred of
                Just cred ->
                    [ credHeader cred ]

                Nothing ->
                    []
        , body = Http.emptyBody
        , timeout = Nothing
        , withCredentials = False
        }



