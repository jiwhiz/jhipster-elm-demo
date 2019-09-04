module Account.Register exposing (Model, Msg(..), init, update, view)

import Account.Api.Request exposing (registerAccount)
import Account.Common exposing (UiElement, sharedForms, toContext, tt)
import Account.I18n.Phrases as AccountPhrases
import Account.I18n.Translator exposing (translator)
import Browser.Navigation exposing (pushUrl)
import Element exposing (Element, alignLeft, fill, height, paddingXY, spacing, width)
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import Shared.ResponsiveUtils exposing (wrapContent)
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty.Defaults
import UiFramework exposing (flatMap, toElement, uiColumn)
import UiFramework.Form.ComposableForm as ComposableForm
import UiFramework.Form.WebForm as WebForm
import UiFramework.Typography exposing (h1)


type alias Model =
    WebForm.WebFormState Values


type alias Values =
    { username : String
    , email : String
    , password : String
    , repeatPassword : String
    , languageKey : String
    }


type Msg
    = NavigateTo Route
    | FormChanged Model
    | Register String String String String
    | RegisterResponse (WebData ())


init : ( Model, Cmd Msg )
init =
    ( Values "" "" "" "" "en" |> WebForm.idle
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

        Register username email password languageKey ->
            let
                registerVM =
                    { username = Just username
                    , email = Just email
                    , password = Just password
                    , languageKey = Just languageKey
                    }
            in
            case model.status of
                WebForm.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    ( { model | status = WebForm.Loading }
                    , registerAccount registerVM RegisterResponse
                    , NoUpdate
                    )

        RegisterResponse (RemoteData.Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            translate AccountPhrases.RegistrationFailed

                        _ ->
                            translate AccountPhrases.ServerError
            in
            ( { model | status = WebForm.Error errorString }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error (translate AccountPhrases.Error) errorString
            )

        RegisterResponse (RemoteData.Success ()) ->
            ( { model | status = WebForm.Idle }
            , Cmd.none
            , ShowToast <|
                Toasty.Defaults.Success
                    (translate AccountPhrases.Success)
                    (translate AccountPhrases.RegistrationSuccess)
            )

        RegisterResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Registration"
    , toElement (toContext sharedState) (content model)
    )


content : Model -> UiElement Msg
content model =
    uiColumn
        [ width fill
        , height fill
        , alignLeft
        , paddingXY 20 10
        , spacing 20
        ]
        [ h1 [ paddingXY 0 30 ] <|
            tt AccountPhrases.RegisterTitle
        , flatMap
            (\context ->
                let
                    fields =
                        sharedForms context
                in
                WebForm.simpleForm
                    FormChanged
                    (ComposableForm.succeed Register
                        |> ComposableForm.append fields.usernameField
                        |> ComposableForm.append fields.emailField
                        |> ComposableForm.append
                            (ComposableForm.succeed (\password _ -> password)
                                |> ComposableForm.append fields.passwordField
                                |> ComposableForm.append fields.repeatPasswordField
                            )
                        |> ComposableForm.append fields.languageField
                    )
                    |> WebForm.withSubmitLabel (context.translate AccountPhrases.RegisterButtonLabel)
                    |> WebForm.withLoadingLabel (context.translate AccountPhrases.RegisterButtonLoading)
                    |> WebForm.view model
            )
        ]
        |> wrapContent
