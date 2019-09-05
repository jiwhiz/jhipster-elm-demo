module Account.Settings exposing (Model, Msg(..), init, update, view)

import Account.Api.Request exposing (updateSettings)
import Account.Common exposing (UiElement, sharedForms, toContext, tt)
import Account.I18n.Phrases as AccountPhrases
import Account.I18n.Translator exposing (translator)
import Browser.Navigation exposing (pushUrl)
import Element exposing (Element, alignLeft, fill, height, paddingXY, spacing, width)
import Http
import LocalStorage exposing (Event(..))
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import Shared.Api.User exposing (User)
import Shared.ResponsiveUtils exposing (wrapContent)
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..), getUsername)
import Toasty.Defaults
import UiFramework
import UiFramework.Form.ComposableForm as ComposableForm
import UiFramework.Form.WebForm as WebForm
import UiFramework.Typography exposing (h1)


type alias Model =
    WebForm.WebFormState Values


type alias Values =
    { firstName : String
    , lastName : String
    , email : String
    , languageKey : String
    }


type Msg
    = NavigateTo Route
    | FormChanged Model
    | SaveSettings String String String String
    | SaveSettingsResponse (WebData ())


init : User -> ( Model, Cmd Msg )
init user =
    ( Values
        (user.firstName |> Maybe.withDefault "")
        (user.lastName |> Maybe.withDefault "")
        user.email
        user.languageKey
        |> WebForm.idle
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

        SaveSettings firstName lastName email languageKey ->
            let
                settings =
                    { username = getUsername sharedState
                    , firstName = firstName
                    , lastName = lastName
                    , email = email
                    , languageKey = languageKey
                    }
            in
            case model.status of
                WebForm.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    ( { model | status = WebForm.Loading }
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
            ( { model | status = WebForm.Error errorString }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error (translate AccountPhrases.Error) errorString
            )

        SaveSettingsResponse (RemoteData.Success ()) ->
            ( { model | status = WebForm.Idle }
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
    , UiFramework.toElement (toContext sharedState) (content (getUsername sharedState) model)
    )


content : String -> Model -> UiElement Msg
content username model =
    UiFramework.uiColumn
        [ width fill
        , height fill
        , alignLeft
        , paddingXY 20 10
        , spacing 20
        ]
        [ h1 [ paddingXY 0 30 ] <|
            tt <|
                AccountPhrases.SettingsTitle username
        , UiFramework.withContext
            (\context ->
                let
                    fields =
                        sharedForms context
                in
                WebForm.simpleForm
                    FormChanged
                    (ComposableForm.succeed SaveSettings
                        |> ComposableForm.append fields.firstNameField
                        |> ComposableForm.append fields.lastNameField
                        |> ComposableForm.append fields.emailField
                        |> ComposableForm.append fields.languageField
                    )
                    |> WebForm.withSubmitLabel (context.translate AccountPhrases.SaveButtonLabel)
                    |> WebForm.withLoadingLabel (context.translate AccountPhrases.SaveButtonLoading)
                    |> WebForm.view model
            )
        ]
        |> wrapContent
