module Pages.Activation exposing (Model, Msg(..), init, update, view)

import Api.Request.Account exposing (activateAccount)
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
    , activationState : ActivationState
    }

type ActivationState
    = NoKey
    | Activating
    | Succeeded
    | Failed String


type Msg
    = NavigateTo Route
    | ActivateResponse (WebData ())


init : Maybe String -> ( Model, Cmd Msg )
init maybeKey =
    case maybeKey of
        Nothing ->
            ( { key = Nothing
              , activationState = NoKey
              }
            , Cmd.none
            )

        Just key ->
            ( { key = Just key
              , activationState = Activating
              }
            , activateAccount key ActivateResponse
            )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (routeToUrlString route), NoUpdate )


        ActivateResponse (RemoteData.Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            "Your user account could not be activated."

                        _ ->
                            "Something went wrong"
            in            
            ( { model | activationState = Failed errorString }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error "Activate Account Error" errorString
            )

        ActivateResponse (RemoteData.Success ()) ->
            ( { model |  activationState = Succeeded }
            , Cmd.none
            , ShowToast <|
                Toasty.Defaults.Success
                    "Success"
                    "Your user account has been activated."
            )

        ActivateResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : Model -> ( String, Element Msg )
view model =
    ( "Activation"
    , el
        [ height fill
        , centerX
        , paddingXY 10 10
        , Font.size 28
        , Font.color (rgb255 59 59 59) -- warning color
        , Font.light
        ]
        ( case model.activationState of
            NoKey ->
                ( text "The activation key is missing." )

            Activating ->
                ( text "Activating." )

            Succeeded ->
                ( paragraph []
                    [ text "Your user account has been activated. " 
                    ,  Input.button [ Font.bold ]
                        { onPress = Just <| NavigateTo Login
                        , label = text "You can login now."
                        }
                    ]
                )

            Failed err ->
                ( paragraph []
                    [ text err 
                    ,  Input.button [ Font.bold ]
                        { onPress = Just <| NavigateTo Register
                        , label = text "Please use the registration form to sign up."
                        }
                    ]
                )
   
        )
    )
