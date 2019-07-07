module Pages.Register exposing (..)

import Api.Data.RegisterVM exposing(RegisterVM)
import Api.Request.Account exposing(registerAccount)
import Api.Request.Auth exposing(authenticationPost)
import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Form exposing (Form)
import Form.View
import Http
import LocalStorage exposing (Event(..), jwtAuthenticationTokenKey)
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty.Defaults
import Validate exposing (Validator, ifBlank, validate)
import UiFramework.Form
import Utils


type alias Model = 
    Form.View.Model Values


type alias Values =
    { username : String
    , email : String
    , password : String
    , repeatPassword : String
    }


type Msg 
    = NavigateTo Route
    | FormChanged Model
    | Register String String String
    | RegisterResponse (WebData ())


init : ( Model, Cmd Msg )
init = 
    ( Values "" "" "" "" |> Form.View.idle
    , Cmd.none
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (routeToUrlString route), NoUpdate )

        FormChanged newModel ->
            ( newModel, Cmd.none, NoUpdate )

        Register username email password ->
            let
                registerVM =
                    { username = Just username
                    , email = Just email
                    , password = Just password
                    }
            in
            case model.state of
                Form.View.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    ( { model | state = Form.View.Loading }
                    , registerAccount registerVM RegisterResponse
                    , NoUpdate
                    )

        RegisterResponse (RemoteData.Failure err) ->
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
            , ShowToast <| Toasty.Defaults.Error "Register Error" errorString
            )

        RegisterResponse (RemoteData.Success ()) ->
            ( { model | state = Form.View.Idle }
            , Cmd.none
            , NoUpdate
            )

        RegisterResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Registration"
    , el 
        [ height fill, centerX, paddingXY 10 10]
        ( content sharedState model )
    )


content : SharedState -> Model -> Element Msg
content sharedState model =
    column
        [ width fill
        , height fill
        , alignLeft
        , paddingXY 20 10
        , spacing 20
        ]
        [ subTitle sharedState
        , registerFormView model
        ]


subTitle : SharedState -> Element Msg
subTitle sharedState =
    el
        [ alignLeft
        , paddingXY 0 10
        , Font.size 30
        , Font.color (rgb255 59 59 59)
        , Font.bold
        ]
        (text "Registration"
        )


registerFormView : Model -> Element Msg
registerFormView model =
    UiFramework.Form.layout
        { onChange = FormChanged
        , action = "Register"
        , loading = "Sending..."
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

        emailField =
            Form.textField
                { parser = Ok
                , value = .email
                , update = \value values -> { values | email = value }
                , attributes =
                    { label = "Email"
                    , placeholder = "Your email"
                    }
                }

        passwordField =
            Form.passwordField
                { parser = Ok
                , value = .password
                , update = \value values -> { values | password = value }
                , attributes =
                    { label = "New password"
                    , placeholder = "New password"
                    }
                }

        repeatPasswordField =
            Form.meta
                (\values ->
                    Form.passwordField
                        { parser =
                            \value ->
                                if value == values.password then
                                    Ok ()

                                else
                                    Err "The passwords do not match"
                        , value = .repeatPassword
                        , update =
                            \newValue values_ ->
                                { values_ | repeatPassword = newValue }
                        , attributes =
                            { label = "New password confirmation"
                            , placeholder = "Conform the new password"
                            }
                        }
                )
    in
    Form.succeed Register
        |> Form.append usernameField
        |> Form.append emailField
        |> Form.append 
            (Form.succeed (\password _ -> password)
                |> Form.append passwordField
                |> Form.append repeatPasswordField
            )


