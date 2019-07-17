module Pages.PasswordUpdate exposing (Model, Msg(..), Values, content, form, init, update, view)

import Api.Request.Account exposing (updatePassword)
import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Form exposing (Form)
import Form.View
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty.Defaults
import UiFramework.Form
import UiFramework.Padding
import UiFramework.Toasty
import Utils
import Validate exposing (Validator, ifBlank, validate)


type alias Model =
    Form.View.Model Values


type alias Values =
    { currentPassword : String
    , password : String
    , repeatPassword : String
    }


type Msg
    = NavigateTo Route
    | FormChanged Model
    | ChangePassword String String
    | ChangePasswordResponse (WebData ())


init : ( Model, Cmd Msg )
init =
    ( Values "" "" "" |> Form.View.idle
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

        ChangePassword currentPassword newPassword ->
            let
                passwordUpdateVM =
                    { currentPassword = currentPassword
                    , newPassword = newPassword
                    }
            in
            case model.state of
                Form.View.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    ( { model | state = Form.View.Loading }
                    , updatePassword sharedState.jwtToken passwordUpdateVM ChangePasswordResponse
                    , NoUpdate
                    )

        ChangePasswordResponse (RemoteData.Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            "An error has occurred! The password could not be changed."

                        _ ->
                            "Something went wrong"
            in
            ( { model | state = Form.View.Error errorString }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error "Change Password Error" errorString
            )

        ChangePasswordResponse (RemoteData.Success ()) ->
            ( { model | state = Form.View.Idle }
            , Cmd.none
            , ShowToast <|
                Toasty.Defaults.Success
                    "Success"
                    "Password changed!"
            )

        ChangePasswordResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Change Password"
    , el
        [ height fill, width fill, paddingXY 10 10 ]
        (content sharedState model)
    )


content : SharedState -> Model -> Element Msg
content sharedState model =
    let
        user =
            el [ Font.bold ] (text (SharedState.displayUsername sharedState))
    in
    column
        [ width fill
        , height fill
        , paddingXY 20 10
        , spacing 20
        , Font.alignLeft
        ]
        [ subTitle sharedState
        , settingsFormView model
        ]
        |> UiFramework.Padding.responsive sharedState


subTitle : SharedState -> Element Msg
subTitle sharedState =
    let
        user =
            el [ Font.bold ] (text (SharedState.displayUsername sharedState))
    in
    el
        [ alignLeft
        , paddingXY 0 30
        , Font.size 30
        , Font.color (rgb255 59 59 59)
        , Font.light
        ]
        (paragraph []
            [ text "Password for ["
            , user
            , text "]"
            ]
        )


settingsFormView : Model -> Element Msg
settingsFormView model =
    UiFramework.Form.layout
        { onChange = FormChanged
        , action = "Save"
        , loading = "Sending request..."
        , validation = Form.View.ValidateOnSubmit
        }
        form
        model


form : Form Values Msg
form =
    let
        currentPasswordField =
            Form.passwordField
                { parser = Ok
                , value = .currentPassword
                , update = \value values -> { values | currentPassword = value }
                , attributes =
                    { label = "Current password"
                    , placeholder = "Current password"
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
                            , placeholder = "Confirm the new password"
                            }
                        }
                )
    in
    Form.succeed ChangePassword
        |> Form.append currentPasswordField
        |> Form.append
            (Form.succeed (\password _ -> password)
                |> Form.append passwordField
                |> Form.append repeatPasswordField
            )
