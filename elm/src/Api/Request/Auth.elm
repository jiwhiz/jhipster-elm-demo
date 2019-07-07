module Api.Request.Auth exposing (authenticationPost)

import Api.Data.JWTToken as JWTToken exposing(JWT)
import Api.Data.LoginVM as LoginVM exposing(LoginVM)
import Api.Endpoint exposing (authentication, unwrap)
import Api.Helper exposing (deleteExpectNothing, post, postExpectNothing)
import Http
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)


authenticationPost : LoginVM -> (WebData JWT -> msg) -> Cmd msg
authenticationPost loginVM toMsg =
    post 
        Nothing
        (unwrap authentication)
        (Http.jsonBody (LoginVM.encoder loginVM))
        toMsg
        JWTToken.decoder
