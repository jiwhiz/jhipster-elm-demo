module Modules.Account.Settings exposing (..)

import Api.Data.Settings exposing(Settings)
import Api.Request.Account exposing(updateSettings)
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
import Modules.Account.I18n.Phrases as AccountPhrases
import Modules.Account.I18n.Translator exposing(translator)
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..), getUsername)
import Toasty.Defaults
import Validate exposing (Validator, ifBlank, validate)
import UiFramework.Form
import UiFramework.Typography exposing (h1)
import Utils


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
    let
        translate =
            translator sharedState.language
    in
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (routeToUrlString route), NoUpdate )

        FormChanged newModel ->
            ( newModel, Cmd.none, NoUpdate )

        SaveSettings firstName lastName email ->
            let
                settings : Settings
                settings =
                    { username = SharedState.getUsername sharedState
                    , firstName = firstName
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
                            translate AccountPhrases.CannotSaveSettings

                        _ ->
                            translate AccountPhrases.ServerError
            in
            ( { model | state = Form.View.Error errorString }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error (translate AccountPhrases.Error) errorString
            )

        SaveSettingsResponse (RemoteData.Success ()) ->
            ( { model | state = Form.View.Idle }
            , Cmd.none
            , ShowToast <| 
                Toasty.Defaults.Success
                    (translate AccountPhrases.Success)
                    (translate AccountPhrases.SaveSuccess)
            )

        SaveSettingsResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Settings"
    , el
        [ width fill, height fill, centerX, paddingXY 100 10]
        ( content sharedState model )
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
        , alignLeft
        , paddingXY 20 10
        , spacing 20
        ]
        [ h1 [paddingXY 0 30]
            (text <| translate <| AccountPhrases.SettingsTitle (SharedState.getUsername sharedState))
        , UiFramework.Form.layout
            { onChange = FormChanged
            , action = translate AccountPhrases.SaveButtonLabel
            , loading = translate AccountPhrases.SaveButtonLoading
            , validation = Form.View.ValidateOnSubmit
            }
            (form sharedState)
            model
        ]


form : SharedState -> Form Values Msg
form sharedState =
    let
        translate =
            translator sharedState.language

        firstNameField =
            Form.textField
                { parser = Ok
                , value = .firstName
                , update = \value values -> { values | firstName = value }
                , attributes =
                    { label = translate AccountPhrases.FirstnameLabel
                    , placeholder = translate AccountPhrases.FirstnamePlaceholder
                    }
                }

        lastNameField =
            Form.textField
                { parser = Ok
                , value = .lastName
                , update = \value values -> { values | lastName = value }
                , attributes =
                    { label = translate AccountPhrases.LastnameLabel
                    , placeholder = translate AccountPhrases.LastnamePlaceholder
                    }
                }

        emailField =
            Form.textField
                { parser = Ok
                , value = .email
                , update = \value values -> { values | email = value }
                , attributes =
                    { label = translate AccountPhrases.EmailLabel
                    , placeholder = translate AccountPhrases.EmailPlaceholder
                    }
                }

    in
    Form.succeed SaveSettings
        |> Form.append firstNameField
        |> Form.append lastNameField
        |> Form.append emailField

