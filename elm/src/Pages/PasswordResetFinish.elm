module Pages.PasswordResetFinish exposing (Model, Msg(..), Values, content, form, init, update, view)

import Api.Data.KeyAndPasswordVM exposing(KeyAndPasswordVM)
import Api.Request.Account exposing (resetPassword)
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
import UiFramework.Toasty
import Utils
import Validate exposing (Validator, ifBlank, validate)


type alias Model =
    { key : Maybe String
    , resetForm : Form.View.Model Values
    }


type alias Values =
    { password : String
    , repeatPassword : String
    }


type Msg
    = NavigateTo Route
    | FormChanged (Form.View.Model Values)
    | ResetPassword String
    | ResetResponse (WebData ())


init : Maybe String -> ( Model, Cmd Msg )
init key =
    ( { key = key
      , resetForm = Values "" "" |> Form.View.idle
      }
    , Cmd.none
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (routeToUrlString route), NoUpdate )

        FormChanged newFormModel ->
            ( { model | resetForm = newFormModel }
            , Cmd.none
            , NoUpdate
            )

        ResetPassword password ->
            case model.resetForm.state of
                Form.View.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    let
                        oldResetForm = model.resetForm

                        newResetForm = { oldResetForm | state = Form.View.Loading }

                        keyAndPasswordVM =
                            { key = model.key
                            , newPassword = Just password
                            }
                    in
                    ( { model | resetForm = newResetForm }
                    , resetPassword keyAndPasswordVM ResetResponse
                    , NoUpdate
                    )

        ResetResponse (RemoteData.Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            "Your password couldn't be reset. Remember a password request is only valid for 24 hours."

                        _ ->
                            "Something went wrong"
            in
            let
                oldResetForm = model.resetForm

                newResetForm = { oldResetForm | state = Form.View.Error errorString }
            in
            
            ( { model | resetForm = newResetForm }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error "Reset Password Error" errorString
            )

        ResetResponse (RemoteData.Success ()) ->
            let
                oldResetForm = model.resetForm

                newResetForm = { oldResetForm | state = Form.View.Idle }
            in
            ( { model |  resetForm = newResetForm }
            , Utils.perform <| NavigateTo Login
            , ShowToast <|
                Toasty.Defaults.Success
                    "Reset Password Succeeded"
                    "<strong>Your password has been reset.</strong> Please login with new password."
            )

        ResetResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : Model -> ( String, Element Msg )
view model =
    ( "Reset"
    , el
        [ height fill, centerX, paddingXY 10 10 ]
        ( case model.key of
            Nothing ->
                el
                    [ paddingXY 30 30
                    , Font.size 28
                    , Font.color (rgb255 59 59 59) -- warning color
                    , Font.light
                    ]
                    ( text "The reset key is missing." )
            Just k ->
                content model
        )
    )


content : Model -> Element Msg
content model =
    column
        [ width fill
        , height fill
        , paddingXY 20 10
        , spacing 20
        , Font.alignLeft
        ]
        [ el
            [ paddingXY 0 30
            , Font.size 28
            , Font.color (rgb255 59 59 59)
            , Font.light
            ]
            (text "Reset your password")
        , UiFramework.Form.layout
            { onChange = FormChanged
            , action = "Validate new password"
            , loading = "Submitting..."
            , validation = Form.View.ValidateOnSubmit
            }
            form
            model.resetForm
        ]


form : Form Values Msg
form =
    let
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
    Form.succeed ResetPassword
        |> Form.append
            (Form.succeed (\password _ -> password)
                |> Form.append passwordField
                |> Form.append repeatPasswordField
            )
