module Api.Request.Auth exposing (authenticationPost)

import Api.Data.JWTToken as JWTToken exposing (JWT)
import Api.Data.LoginVM as LoginVM exposing (LoginVM)
import Api.Endpoint exposing (authentication, unwrap)
import Api.Helper exposing (post)
import Http
import RemoteData exposing (WebData)


authenticationPost : LoginVM -> (WebData JWT -> msg) -> Cmd msg
authenticationPost loginVM toMsg =
    post
        Nothing
        (unwrap authentication)
        (Http.jsonBody (LoginVM.encoder loginVM))
        toMsg
        JWTToken.decoder
