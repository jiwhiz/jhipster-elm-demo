module SharedState exposing (SharedState, SharedStateUpdate(..), update, displayUsername)

import Api.Data.Role exposing (Role(..))
import Api.Data.User as User exposing (User)
import Browser.Navigation
import Element exposing (Device)
import I18n.I18n exposing (Language(..))
import Time exposing (Posix, Zone)
import Toasty.Defaults


type alias SharedState =
    { navKey : Browser.Navigation.Key
    , currentTime : Maybe Posix
    , timezone : Maybe Zone
    , language : Language
    , device : Device
    , jwtToken : Maybe String
    , user : Maybe User
    }


type SharedStateUpdate
    = NoUpdate
    | UpdateTime Posix
    | UpdateTimezone Zone
    | UpdateLanguage Language
    | UpdateDevice Device
    | UpdateUser User
    | RefreshLogin
    | ShowToast Toasty.Defaults.Toast
    | UpdateJwtToken (Maybe String) Bool


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    case sharedStateUpdate of
        UpdateTime time ->
            { sharedState | currentTime = Just time }

        UpdateTimezone zone ->
            { sharedState | timezone = Just zone }

        UpdateLanguage language ->
            { sharedState | language = language}

        UpdateDevice device ->
            { sharedState | device = device }

        UpdateUser user ->
            -- Received for a positive login
            { sharedState | user = Just user }

        UpdateJwtToken maybeJwt rememberMe ->
            case maybeJwt of
                Nothing ->
                    { sharedState | jwtToken = Nothing, user = Nothing }

                Just token ->
                    { sharedState | jwtToken = Just token }

        NoUpdate ->
            sharedState

        RefreshLogin ->
            sharedState

        ShowToast _ ->
            sharedState


displayUsername : SharedState -> String
displayUsername sharedState =
    case sharedState.user of
        Nothing ->
            ""

        Just user ->
            user.username
