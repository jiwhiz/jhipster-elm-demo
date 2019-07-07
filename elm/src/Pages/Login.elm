module Pages.Login exposing (Model, Msg(..), Values, init, update, view)

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
import LocalStorage exposing (jwtAuthenticationTokenKey)
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty.Defaults
import UiFramework.Alert as Alert
import UiFramework.Form
import UiFramework.Typography
import UiFramework.Types exposing (Role(..), ScreenSize(..))
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
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            "Wrong Password or Username!"

                        Http.BadStatus 422 ->
                            "Your email is not confirmed!"

                        _ ->
                            "Something went wrong"
            in
            ( { model | state = Form.View.Error errorString }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error "Error" errorString
            )

        LoginResponse (RemoteData.Success jwt) ->
            ( { model | state = Form.View.Idle }
            , Utils.perform <|
                ShowToastAndRedirect <|
                    Toasty.Defaults.Success "Logged in" "You are now logged in."
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
    ( "Login"
    , el
        [ height fill, centerX, paddingXY 10 10 ]
        ( case sharedState.user of
            Just user ->
                el
                    [ paddingXY 30 30
                    , Font.size 28
                    , Font.color (rgb255 59 59 59)
                    , Font.light
                    ]
                    (text <| "You are logged in as " ++ user.username)

            Nothing ->
                content sharedState model
        )
    )


content : SharedState -> Model -> Element Msg
content sharedState model =
    column
        [ width fill
        , height fill
        , paddingXY 20 10
        , spacing 20
        , Font.alignLeft
        , Font.size 16
        ]
        [ el
            [ paddingXY 0 30
            , Font.alignLeft
            , Font.size 28
            , Font.color (rgb255 59 59 59)
            , Font.light
            ]
            ( text "Sign in" )
        , loginFormView model
        , Alert.simple Warning <|
            ( Alert.link Warning
                { onPress = Just <| NavigateTo PasswordResetRequest
                , label = text "Did you forget your password?"
                }
            )
        , Alert.simple Warning <|
            paragraph
                [ Font.alignLeft ]
                [ text "You don't have an account yet? "
                , Alert.link Warning
                    { onPress = Just <| NavigateTo Register
                    , label = text "Register a new account"
                    }
                ]
        ]


loginFormView : Form.View.Model Values -> Element Msg
loginFormView model =
    UiFramework.Form.layout
        { onChange = FormChanged
        , action = "Login"
        , loading = "Logging in..."
        , validation = Form.View.ValidateOnSubmit
        }
        form
        model


form : Form Values Msg
form =
    let
        usernameField =
            Form.textField
                { parser = Ok
                , value = .username
                , update = \value values -> { values | username = value }
                , attributes =
                    { label = "Username"
                    , placeholder = "Your username"
                    }
                }

        passwordField =
            Form.passwordField
                { parser = Ok
                , value = .password
                , update = \value values -> { values | password = value }
                , attributes =
                    { label = "Password"
                    , placeholder = "Your password"
                    }
                }

        rememberMeCheckbox =
            Form.checkboxField
                { parser = Ok
                , value = .rememberMe
                , update = \value values -> { values | rememberMe = value }
                , attributes =
                    { label = "Remember me" }
                }
    in
    Form.succeed Login
        |> Form.append usernameField
        |> Form.append passwordField
        |> Form.append rememberMeCheckbox
