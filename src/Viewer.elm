module Viewer exposing (Viewer(..), avatar, cred, decoder, store, username)

-- import User exposing (UserData,Username,userdecode)

import Api exposing (Cred)
import Avatar exposing (Avatar, decoder)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (custom, optional, required)
import Username exposing (Username)


type Viewer
    = Viewer Avatar Cred


cred : Viewer -> Cred
cred (Viewer _ val) =
    val


username : Viewer -> Username
username (Viewer _ val) =
    Api.username val


store : Viewer -> Cmd msg
store (Viewer avatarVal credVal) =
    Api.storeCredWith
        credVal
        avatarVal


avatar : Viewer -> Avatar
avatar (Viewer val _) =
    val


decoder : Decoder (Cred -> Viewer)
decoder =
    Decode.succeed Viewer
        |> custom (Decode.field "user" Avatar.decoder)
