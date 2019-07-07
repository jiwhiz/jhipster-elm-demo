module Api.Request.Account exposing (..)

import Api.Data.User as User exposing(User)
import Api.Data.KeyAndPasswordVM as KeyAndPasswordVM exposing(KeyAndPasswordVM)
import Api.Data.PasswordUpdateVM as PasswordUpdateVM exposing(PasswordUpdateVM)
import Api.Data.RegisterVM as RegisterVM exposing(RegisterVM)
import Api.Data.Settings as Settings exposing(Settings)
import Api.Endpoint as Endpoint exposing (unwrap)
import Api.Helper exposing (get, getExpectNothing, postExpectNothing)
import Http
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)


getCurrentAccount : Maybe String -> (WebData User -> msg) -> Cmd msg
getCurrentAccount token toMsg =
    get
        token 
        (unwrap Endpoint.account)
        toMsg
        User.decoder


registerAccount : RegisterVM -> (WebData () -> msg) -> Cmd msg
registerAccount registerVM toMsg =
    postExpectNothing
        Nothing
        (unwrap Endpoint.register)
        (Http.jsonBody <| RegisterVM.encoder registerVM)
        toMsg


activateAccount : String -> (WebData () -> msg) -> Cmd msg
activateAccount key toMsg =
    getExpectNothing
        Nothing
        (unwrap Endpoint.activate ++ "?key=" ++ key)
        toMsg



requestResetPassword : String -> (WebData () -> msg) -> Cmd msg
requestResetPassword email toMsg =
    postExpectNothing
        Nothing
        (unwrap Endpoint.resetPasswordRequest)
        (Http.stringBody "text/plain" email)
        toMsg


resetPassword : KeyAndPasswordVM -> (WebData () -> msg) -> Cmd msg
resetPassword keyAndPasswordVM toMsg =
    postExpectNothing
        Nothing
        (unwrap Endpoint.resetPassword)
        (Http.jsonBody <| KeyAndPasswordVM.encoder keyAndPasswordVM)
        toMsg


updateSettings : Maybe String -> Settings -> (WebData () -> msg) -> Cmd msg
updateSettings token settings toMsg =
    postExpectNothing
        token
        (unwrap Endpoint.account)
        (Http.jsonBody <| Settings.encoder settings)
        toMsg


updatePassword : Maybe String -> PasswordUpdateVM -> (WebData () -> msg) -> Cmd msg
updatePassword token passwordUpdateVM toMsg =
    postExpectNothing
        token
        (unwrap Endpoint.changePassword)
        (Http.jsonBody <| PasswordUpdateVM.encoder passwordUpdateVM)
        toMsg


