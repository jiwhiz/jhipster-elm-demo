module Pages.Settings exposing (Model, Msg(..), Values, content, form, init, settingsFormView, subTitle, update, view)

import Api.Data.Settings exposing (Settings)
import Api.Request.Account exposing (updateSettings)
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
import SharedState exposing (SharedState, SharedStateUpdate(..), displayUsername)
import Toasty.Defaults
import UiFramework.Form
import UiFramework.Padding
import Utils
import Validate exposing (Validator, ifBlank, validate)


type alias Model =
    Form.View.Model Values


type alias Values =
    { firstName : String
    , lastName : String
    , email : String

    -- , language : String  TODO: add language after i18n
    }


type Msg
    = NavigateTo Route
    | FormChanged Model
    | SaveSettings String String String
    | SaveSettingsResponse (WebData ())


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
            ( newModel, Cmd.none, NoUpdate )

        SaveSettings firstName lastName email ->
            let
                settings : Settings
                settings =
                    { firstName = firstName
                    , lastName = lastName
                    , email = email
                    }
            in
            case model.state of
                Form.View.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    ( { model | state = Form.View.Loading }
                    , updateSettings sharedState.jwtToken settings SaveSettingsResponse
                    , NoUpdate
                    )

        SaveSettingsResponse (RemoteData.Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            "Email is already in use!"

                        _ ->
                            "An error has occurred! Settings could not be saved."
            in
            ( { model | state = Form.View.Error errorString }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error "Error" errorString
            )

        SaveSettingsResponse (RemoteData.Success ()) ->
            ( { model | state = Form.View.Idle }
            , Cmd.none
            , ShowToast <|
                Toasty.Defaults.Success
                    "Success"
                    "Settings saved!"
            )

        SaveSettingsResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Settings"
    , el
        [ height fill, width fill, paddingXY 10 10 ]
        (content sharedState model)
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
            [ text "User settings for ["
            , user
            , text "]"
            ]
        )


settingsFormView : Model -> Element Msg
settingsFormView model =
    UiFramework.Form.layout
        { onChange = FormChanged
        , action = "Save"
        , loading = "Submitting..."
        , validation = Form.View.ValidateOnSubmit
        }
        form
        model


form : Form Values Msg
form =
    let
        firstNameField =
            Form.textField
                { parser = Ok
                , value = .firstName
                , update = \value values -> { values | firstName = value }
                , attributes =
                    { label = "First Name"
                    , placeholder = "Your first name"
                    }
                }

        lastNameField =
            Form.textField
                { parser = Ok
                , value = .lastName
                , update = \value values -> { values | lastName = value }
                , attributes =
                    { label = "Last Name"
                    , placeholder = "Your last name"
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
    in
    Form.succeed SaveSettings
        |> Form.append firstNameField
        |> Form.append lastNameField
        |> Form.append emailField
