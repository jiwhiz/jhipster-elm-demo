module Admin.UserManagement.Api.Request exposing (loadUsers)

import Admin.UserManagement.Api.User as User exposing (UserDTO)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)
import Shared.Api.Endpoint as Endpoint exposing (unwrap)
import Shared.Api.Helper exposing (get)


loadUsers : Maybe String -> (WebData (List UserDTO) -> msg) -> Cmd msg
loadUsers token toMsg =
    get
        token
        (unwrap Endpoint.users)
        toMsg
        (Decode.list User.decoder)
