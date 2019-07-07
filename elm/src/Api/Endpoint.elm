module Api.Endpoint exposing
    ( Endpoint(..)
    , unwrap
    , authentication
    , account
    , register
    , activate
    , resetPasswordRequest
    , resetPassword
    , changePassword
    )

import Url.Builder exposing (QueryParameter)


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


basePath : String
basePath =
    "http://localhost:8080"


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin basePath
        ("api" :: paths)
        queryParams
        |> Endpoint


authentication : Endpoint
authentication =
    url [ "authenticate" ] []


account : Endpoint
account =
    url [ "account" ] []


register : Endpoint
register =
    url [ "register" ] []


activate : Endpoint
activate =
    url [ "activate" ] []


resetPasswordRequest : Endpoint
resetPasswordRequest =
    url [ "account", "reset-password", "init"] []


resetPassword : Endpoint
resetPassword =
    url [ "account", "reset-password", "finish"] []


changePassword : Endpoint
changePassword =
    url [ "account", "change-password"] []
