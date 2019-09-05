module Login.Login exposing (Model, Msg(..), Values, init, update, view)

import Browser.Navigation exposing (pushUrl)
import Element exposing (Element, fill, height, paddingXY, spacing, width)
import Element.Font as Font
import Login.Api.JWTToken exposing (JWT)
import Login.Api.Request exposing (authenticate)
import Login.Common exposing (UiElement, toContext, tt)
import Login.I18n.Phrases as LoginPhrases
import Login.I18n.Translator exposing (translator)
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import Shared.I18n exposing (Language(..))
import Shared.ResponsiveUtils exposing (wrapContent)
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty.Defaults
import UiFramework
import UiFramework.Alert as Alert
import UiFramework.Form.CheckboxField as CheckboxField
import UiFramework.Form.ComposableForm as ComposableForm
import UiFramework.Form.TextField as TextField
import UiFramework.Form.WebForm as WebForm
import UiFramework.Types exposing (Role(..))
import UiFramework.Typography exposing (h1)
import Utils


type alias Model =
    WebForm.WebFormState Values


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
    ( Values "" "" False |> WebForm.idle
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
            case model.status of
                WebForm.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    ( { model | status = WebForm.Loading }
                    , authenticate loginVM LoginResponse
                    , NoUpdate
                    )

        LoginResponse (RemoteData.Failure _) ->
            ( { model | status = WebForm.Error (translate LoginPhrases.FailedLogin) }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error (translate LoginPhrases.Error) (translate LoginPhrases.FailedLogin)
            )

        LoginResponse (RemoteData.Success jwt) ->
            ( { model | status = WebForm.Idle }
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
    ( "Login"
    , UiFramework.toElement (toContext sharedState) (content model)
    )


content : Model -> UiElement Msg
content model =
    UiFramework.uiColumn
        [ width fill
        , height fill
        , paddingXY 20 10
        , spacing 20
        , Font.alignLeft
        ]
        [ h1 [ paddingXY 0 30 ] <|
            tt LoginPhrases.LoginTitle
        , UiFramework.withContext
            (\context ->
                case context.user of
                    Just user ->
                        Alert.simple Warning <|
                            (tt <| LoginPhrases.LoggedInAs user.username)

                    Nothing ->
                        loginPanel model
            )
        ]
        |> wrapContent


loginPanel : Model -> UiElement Msg
loginPanel model =
    UiFramework.uiColumn
        [ width fill
        , height fill
        , spacing 20
        , Font.alignLeft
        ]
        [ UiFramework.withContext
            (\context ->
                let
                    usernameField =
                        ComposableForm.textField
                            { parser = Ok
                            , value = .username
                            , update = \value values -> { values | username = value }
                            , error = always Nothing
                            , attributes =
                                TextField.defaultAttributes
                                    |> TextField.withLabel (context.translate LoginPhrases.UsernameLabel)
                                    |> TextField.withPlaceholder (context.translate LoginPhrases.UsernamePlaceholder)
                            }

                    passwordField =
                        ComposableForm.textField
                            { parser = Ok
                            , value = .password
                            , update = \value values -> { values | password = value }
                            , error = always Nothing
                            , attributes =
                                TextField.defaultAttributes
                                    |> TextField.withLabel (context.translate LoginPhrases.PasswordLabel)
                                    |> TextField.withPlaceholder (context.translate LoginPhrases.PasswordPlaceholder)
                            }

                    rememberMeCheckbox =
                        ComposableForm.checkboxField
                            { parser = Ok
                            , value = .rememberMe
                            , update = \value values -> { values | rememberMe = value }
                            , error = always Nothing
                            , attributes =
                                CheckboxField.defaultAttributes
                                    |> CheckboxField.withLabel (context.translate LoginPhrases.RememberMeLabel)
                            }
                in
                WebForm.simpleForm
                    FormChanged
                    (ComposableForm.succeed Login
                        |> ComposableForm.append usernameField
                        |> ComposableForm.append passwordField
                        |> ComposableForm.append rememberMeCheckbox
                    )
                    |> WebForm.withSubmitLabel (context.translate LoginPhrases.SignInButtonLabel)
                    |> WebForm.withLoadingLabel (context.translate LoginPhrases.SignInLoadingLabel)
                    |> WebForm.view model
            )
        , Alert.simple Warning <|
            Alert.link
                { onPress = Just <| NavigateTo PasswordResetRequest
                , label = tt LoginPhrases.ForgetPassword
                }
        , Alert.simple Warning <|
            UiFramework.uiParagraph
                [ Font.alignLeft ]
                [ tt LoginPhrases.NoAccountYet
                , UiFramework.uiText " "
                , Alert.link
                    { onPress = Just <| NavigateTo Register
                    , label = tt LoginPhrases.RegisterNewAccount
                    }
                ]
        ]
