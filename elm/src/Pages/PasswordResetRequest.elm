module Pages.PasswordResetRequest exposing (Model, Msg(..), Values, content, form, init, update, view)

import Api.Request.Account exposing (requestResetPassword)
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
    Form.View.Model Values


type alias Values =
    { email : String
    }


type Msg
    = NavigateTo Route
    | FormChanged Model
    | ResetRequest String
    | ResetResponse (WebData ())


init : ( Model, Cmd Msg )
init =
    ( Values "" |> Form.View.idle
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

        ResetRequest email ->
            case model.state of
                Form.View.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    ( { model | state = Form.View.Loading }
                    , requestResetPassword email ResetResponse
                    , NoUpdate
                    )

        ResetResponse (RemoteData.Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            "<strong>Email address isn't registered!</strong> Please check and try again!"

                        _ ->
                            "Something went wrong"
            in
            ( { model | state = Form.View.Error errorString }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error "Reset Password Error" errorString
            )

        ResetResponse (RemoteData.Success ()) ->
            ( { model | state = Form.View.Idle }
            , Cmd.none
            , ShowToast <|
                Toasty.Defaults.Success
                    "Reset Password Request Sent"
                    "Check your emails for details on how to reset your password."
            )

        ResetResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : Model -> ( String, Element Msg )
view model =
    ( "Reset"
    , el
        [ height fill, centerX, paddingXY 10 10 ]
        (content model)
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
            , action = "Reset password"
            , loading = "Sending request..."
            , validation = Form.View.ValidateOnSubmit
            }
            form
            model
        ]


form : Form Values Msg
form =
    let
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
    in
    Form.succeed ResetRequest
        |> Form.append emailField
