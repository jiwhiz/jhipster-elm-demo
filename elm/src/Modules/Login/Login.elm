module Modules.Login.Login exposing (Model, Msg(..), Values, init, update, view)

import Api.Data.JWTToken exposing (JWT)
import Api.Data.LoginVM exposing (LoginVM)
import Api.Request.Auth exposing (authenticationPost)
import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Form exposing (Form)
import Form.View
import Http
import Modules.Login.I18n.Phrases as LoginPhrases
import Modules.Login.I18n.Translator exposing (translator)
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty.Defaults
import UiFramework.Alert as Alert
import UiFramework.Form
import UiFramework.Padding
import UiFramework.Types exposing (Role(..), ScreenSize(..))
import UiFramework.Typography exposing (h1)
import Utils
import Validate exposing (Validator, ifBlank, validate)


type alias Model =
    Form.View.Model Values


type alias Values =
    { username : String
    , password : String
    , rememberMe : Bool
    }


type Msg
    = NavigateTo Route
    | FormChanged Model
    | Login String String Bool
    | LoginResponse (WebData JWT)
    | ShowToastAndRedirect Toasty.Defaults.Toast


init : ( Model, Cmd Msg )
init =
    ( Values "" "" False |> Form.View.idle
    , Cmd.none
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    let
        translate =
            translator sharedState.language
    in
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (routeToUrlString route), NoUpdate )

        FormChanged newModel ->
            ( newModel
            , Cmd.none
            , NoUpdate
            )

        Login username password rememberMe ->
            let
                loginVM =
                    { username = Just username
                    , password = Just password
                    , rememberMe = rememberMe
                    }
            in
            case model.state of
                Form.View.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    ( { model | state = Form.View.Loading }
                    , authenticationPost loginVM LoginResponse
                    , NoUpdate
                    )

        LoginResponse (RemoteData.Failure err) ->
            ( { model | state = Form.View.Error (translate LoginPhrases.FailedLogin) }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error (translate LoginPhrases.Error) (translate LoginPhrases.FailedLogin)
            )

        LoginResponse (RemoteData.Success jwt) ->
            ( { model | state = Form.View.Idle }
            , Utils.perform <|
                ShowToastAndRedirect <|
                    Toasty.Defaults.Success (translate LoginPhrases.Success) (translate <| LoginPhrases.LoggedIn)
            , UpdateJwtToken (Just jwt.token) model.values.rememberMe
            )

        LoginResponse _ ->
            ( model, Cmd.none, NoUpdate )

        ShowToastAndRedirect toast ->
            ( model
            , Utils.perform <| NavigateTo Home
            , ShowToast toast
            )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    let
        translate =
            translator sharedState.language
    in
    ( "Login"
    , el
        [ height fill
        , width fill
        , centerX
        , paddingXY 100 10
        ]
        (case sharedState.user of
            Just user ->
                el
                    [ paddingXY 30 30
                    , Font.size 28
                    , Font.color (rgb255 59 59 59)
                    , Font.light
                    ]
                    (text <| translate <| LoginPhrases.LoggedInAs user.username)

            Nothing ->
                content sharedState model
        )
    )


content : SharedState -> Model -> Element Msg
content sharedState model =
    let
        translate =
            translator sharedState.language
    in
    column
        [ width fill
        , height fill
        , paddingXY 20 10
        , spacing 20
        , Font.alignLeft
        ]
        [ h1 [ paddingXY 0 30 ]
            (text <| translate LoginPhrases.LoginTitle)
        , UiFramework.Form.layout
            { onChange = FormChanged
            , action = translate LoginPhrases.SignInButtonLabel
            , loading = translate LoginPhrases.SignInLoadingLabel
            , validation = Form.View.ValidateOnSubmit
            }
            (form sharedState)
            model
        , Alert.simple Warning <|
            Alert.link Warning
                { onPress = Just <| NavigateTo PasswordResetRequest
                , label = text <| translate LoginPhrases.ForgetPassword
                }
        , Alert.simple Warning <|
            paragraph
                [ Font.alignLeft ]
                [ text <| translate LoginPhrases.NoAccountYet
                , text " "
                , Alert.link Warning
                    { onPress = Just <| NavigateTo Register
                    , label = text <| translate LoginPhrases.RegisterNewAccount
                    }
                ]
        ]
        |> UiFramework.Padding.responsive sharedState


form : SharedState -> Form Values Msg
form sharedState =
    let
        translate =
            translator sharedState.language

        usernameField =
            Form.textField
                { parser = Ok
                , value = .username
                , update = \value values -> { values | username = value }
                , attributes =
                    { label = translate LoginPhrases.UsernameLabel
                    , placeholder = translate LoginPhrases.UsernamePlaceholder
                    }
                }

        passwordField =
            Form.passwordField
                { parser = Ok
                , value = .password
                , update = \value values -> { values | password = value }
                , attributes =
                    { label = translate LoginPhrases.PasswordLabel
                    , placeholder = translate LoginPhrases.PasswordPlaceholder
                    }
                }

        rememberMeCheckbox =
            Form.checkboxField
                { parser = Ok
                , value = .rememberMe
                , update = \value values -> { values | rememberMe = value }
                , attributes =
                    { label = translate LoginPhrases.RememberMeLabel }
                }
    in
    Form.succeed Login
        |> Form.append usernameField
        |> Form.append passwordField
        |> Form.append rememberMeCheckbox
