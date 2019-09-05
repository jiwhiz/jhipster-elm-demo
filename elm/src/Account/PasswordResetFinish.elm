module Account.PasswordResetFinish exposing (Model, Msg(..), init, update, view)

import Account.Api.Request exposing (resetPassword)
import Account.Common exposing (UiElement, sharedForms, toContext, tt)
import Account.I18n.Phrases as AccountPhrases
import Account.I18n.Translator exposing (translator)
import Browser.Navigation exposing (pushUrl)
import Element exposing (Element, fill, height, paddingXY, spacing, width)
import Element.Font as Font
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty.Defaults
import UiFramework
import UiFramework.Alert as Alert
import UiFramework.Form.ComposableForm as ComposableForm
import UiFramework.Form.WebForm as WebForm
import UiFramework.Types exposing (Role(..))
import UiFramework.Typography exposing (h1)
import Utils


type alias Model =
    { key : Maybe String
    , resetForm : WebForm.WebFormState Values
    }


type alias Values =
    { password : String
    , repeatPassword : String
    }


type Msg
    = NavigateTo Route
    | FormChanged (WebForm.WebFormState Values)
    | ResetPassword String
    | ResetResponse (WebData ())


init : Maybe String -> ( Model, Cmd Msg )
init key =
    ( { key = key
      , resetForm = Values "" "" |> WebForm.idle
      }
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

        FormChanged newFormModel ->
            ( { model | resetForm = newFormModel }
            , Cmd.none
            , NoUpdate
            )

        ResetPassword password ->
            case model.resetForm.status of
                WebForm.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    let
                        oldResetForm =
                            model.resetForm

                        newResetForm =
                            { oldResetForm | status = WebForm.Loading }

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
                            translate AccountPhrases.CannotReset

                        _ ->
                            translate AccountPhrases.ServerError
            in
            let
                oldResetForm =
                    model.resetForm

                newResetForm =
                    { oldResetForm | status = WebForm.Error errorString }
            in
            ( { model | resetForm = newResetForm }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error (translate AccountPhrases.Error) errorString
            )

        ResetResponse (RemoteData.Success ()) ->
            let
                oldResetForm =
                    model.resetForm

                newResetForm =
                    { oldResetForm | status = WebForm.Idle }
            in
            ( { model | resetForm = newResetForm }
            , Utils.perform <| NavigateTo Login
            , ShowToast <|
                Toasty.Defaults.Success
                    (translate AccountPhrases.Success)
                    (translate AccountPhrases.ResetSuccess)
            )

        ResetResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Reset"
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
        [ h1
            [ paddingXY 0 30 ]
          <|
            tt AccountPhrases.ResetPasswordTitle
        , case model.key of
            Nothing ->
                Alert.simple Danger <|
                    tt AccountPhrases.MissingResetKey

            Just _ ->
                UiFramework.withContext
                    (\context ->
                        let
                            fields =
                                sharedForms context
                        in
                        WebForm.simpleForm
                            FormChanged
                            (ComposableForm.succeed ResetPassword
                                |> ComposableForm.append
                                    (ComposableForm.succeed (\password _ -> password)
                                        |> ComposableForm.append fields.passwordField
                                        |> ComposableForm.append fields.repeatPasswordField
                                    )
                            )
                            |> WebForm.withSubmitLabel (context.translate AccountPhrases.ResetButtonLabel)
                            |> WebForm.withLoadingLabel (context.translate AccountPhrases.ResetButtonLoading)
                            |> WebForm.view model.resetForm
                    )
        ]
