module Main exposing (..)

import Api.Data.User as User exposing(User)
import Api.Request.Account exposing (getCurrentAccount)
import Browser
import Browser.Events
import Browser.Navigation
import Element exposing (Device, classifyDevice)
import Html
import I18n.I18n exposing (Language(..), languageFromCode)
import Json.Decode as Decode
import Json.Encode as Encode
import LocalStorage exposing (Event(..), jwtAuthenticationTokenKey)
import Pages.Home
import Pages.Login
import Pages.NotFound
import RemoteData exposing (RemoteData(..), WebData)
import Router
import Routes
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Task
import Time exposing (Posix, Zone)
import Url



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



---- MODEL ----


type alias Model =
    { appState : AppState
    , navKey : Browser.Navigation.Key
    , url : Url.Url
    , sessionJwtToken : Maybe String
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Flags =
    { currentTime : Int
    , winSize : WindowSize
    , jwtToken : Decode.Value
    }


type AppState
    = NotReady (Maybe Posix) (Maybe Zone) Device
    | Ready SharedState Router.Model
    | FailedToInitialize


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        jwtToken = 
            (Decode.decodeValue Decode.string flags.jwtToken |> Result.toMaybe)
    in
    ( { appState =
            NotReady
                (Just (Time.millisToPosix flags.currentTime))
                (Just Time.utc)
                (classifyDevice flags.winSize)
      , url = url
      , navKey = key
      , sessionJwtToken = jwtToken
      }
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , getCurrentAccount jwtToken GetAccountResponse
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        -- [ Time.every 1000 TimeChange
        [ Sub.map StorageEvent LocalStorage.watchChanges
        , Browser.Events.onResize
            (\x y ->
                WindowSizeChange (WindowSize x y)
            )
        , case model.appState of
            Ready sharedState routerModel ->
                Sub.map RouterMsg <| Router.subscriptions sharedState routerModel

            _ ->
                Sub.none
        ]



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | TimeChange Posix
    | AdjustTimeZone Zone
    | RouterMsg Router.Msg
    | StorageEvent LocalStorage.Event
    | WindowSizeChange WindowSize
    | GetAccountResponse (WebData User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Main MSG" msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Browser.Navigation.load url )

        UrlChanged url ->
            updateRouter { model | url = url } (Router.UrlChanged url)

        TimeChange time ->
            updateTime model time

        AdjustTimeZone zone ->
            updateTimeZone model zone

        RouterMsg routerMsg ->
            updateRouter model routerMsg

        StorageEvent event ->
            ( model, Cmd.none )
            -- handleStorageEvent model event

        WindowSizeChange winSize ->
            handleWindowSize model winSize

        GetAccountResponse (RemoteData.Failure err) ->
            -- The JWT Token expired or no token in local storage
            getReady Nothing model

        GetAccountResponse (RemoteData.Success user) ->
            getReady (Just user) model

        GetAccountResponse _ ->
            ( model, Cmd.none )


getReady : Maybe User ->  Model -> ( Model, Cmd Msg )
getReady user model =
    case model.appState of
        NotReady time zone device ->
            let
                initSharedState =
                    { navKey = model.navKey
                    , currentTime = time
                    , timezone = zone
                    , language = getUserLanguage user
                    , device = device
                    , jwtToken = model.sessionJwtToken
                    , user = user
                    }

                ( initRouterModel, routerCmd ) =
                    Router.init model.url
            in
            ( { model | appState = Ready initSharedState initRouterModel }
            , Cmd.map RouterMsg routerCmd
            )

        Ready sharedState routerModel ->
            ( model, Cmd.none )
                |> withErrorLog "Response from getAccount when app state is already Ready!"

        FailedToInitialize ->
            ( model, Cmd.none )


getUserLanguage : Maybe User -> Language
getUserLanguage maybeUser =
    case maybeUser of
        Nothing ->
            English

        Just user ->
            languageFromCode user.language


updateTime : Model -> Posix -> ( Model, Cmd Msg )
updateTime model time =
    case model.appState of
        NotReady _ zone state ->
            ( { model | appState = NotReady (Just time) zone state }
            , Cmd.none
            )

        Ready sharedState routerModel ->
            ( { model | appState = Ready (SharedState.update sharedState (UpdateTime time)) routerModel }
            , Cmd.none
            )

        FailedToInitialize ->
            ( model, Cmd.none )


updateTimeZone : Model -> Zone -> ( Model, Cmd Msg )
updateTimeZone model zone =
    case model.appState of
        NotReady time _ state ->
            ( { model | appState = NotReady time (Just zone) state }
            , Cmd.none
            )

        Ready sharedState routerModel ->
            ( { model | appState = Ready (SharedState.update sharedState (UpdateTimezone zone)) routerModel }
            , Cmd.none
            )

        FailedToInitialize ->
            ( model, Cmd.none )


updateRouter : Model -> Router.Msg -> ( Model, Cmd Msg )
updateRouter model routerMsg =
    case model.appState of
        Ready sharedState routerModel ->
            let
                nextSharedState =
                    SharedState.update sharedState sharedStateUpdate

                ( nextRouterModel, routerCmd, sharedStateUpdate ) =
                    Router.update sharedState routerMsg routerModel
            in
            ( { model | appState = Ready nextSharedState nextRouterModel }
            , Cmd.map RouterMsg routerCmd
            )

        _ ->
            ( model, Cmd.none )


-- handleStorageEvent : Model -> LocalStorage.Event -> ( Model, Cmd Msg )
-- handleStorageEvent model event =
--     let
--         test =
--             Debug.log "handleStorageEvent" event
--     in
--     case event of
--         Updated key value ->
--             if key == jwtAuthenticationTokenKey then
--                 updateJwtToken model value

--             else
--                 ( model, Cmd.none )

--         WriteFailure key value err ->
--             ( model, Cmd.none )
--                 |> withErrorLog
--                     ("Unable to write to localStorage key '"
--                         ++ key
--                         ++ "': "
--                         ++ err
--                     )

--         BadMessage err ->
--             ( model, Cmd.none )
--                 |> withErrorLog ("Malformed storage event: " ++ Decode.errorToString err)


-- updateJwtToken : Model -> Maybe String -> ( Model, Cmd Msg )
-- updateJwtToken model value =
--     case value of
--         Nothing ->
--             resetJwtToken model

--         Just jwtToken ->
--             ( { model | sessionJwtToken = Just jwtToken }
--             , getCurrentAccount (Just jwtToken) GetAccountResponse
--             )


-- resetJwtToken : Model -> ( Model, Cmd Msg )
-- resetJwtToken model =
--     case model.appState of
--         NotReady time zone device ->
--             let
--                 initSharedState =
--                     { navKey = model.navKey
--                     , currentTime = time
--                     , timezone = zone
--                     , device = device
--                     , toggleMenu = False
--                     , accountDropdownState = False
--                     , isLoggedIn = False
--                     , jwtToken = Nothing
--                     , user = Nothing
--                     }

--                 ( initRouterModel, routerCmd ) =
--                     Router.init model.url
--             in
--             ( { model | appState = Ready initSharedState initRouterModel }
--             , Cmd.map RouterMsg routerCmd
--             )

--         Ready sharedState routerModel ->
--             -- user might be logged out, or token expired, so delete jwt token
--             ( { model | appState = Ready (SharedState.update sharedState (UpdateJwtToken Nothing)) routerModel }
--             , Cmd.none
--             )

--         FailedToInitialize ->
--             ( model, Cmd.none )


handleWindowSize : Model -> WindowSize -> ( Model, Cmd Msg )
handleWindowSize model winSize =
    case model.appState of
        NotReady time zone device ->
            ( { model | appState = NotReady time zone (classifyDevice winSize) }
            , Cmd.none
            )

        Ready sharedState routerModel ->
            ( { model | appState = Ready (SharedState.update sharedState (UpdateDevice <| classifyDevice winSize)) routerModel }
            , Cmd.none
            )

        FailedToInitialize ->
            ( model, Cmd.none )


withErrorLog : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withErrorLog err updateTuple =
    updateTuple
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, logError err ])


logError : String -> Cmd Msg
logError error =
    let
        log =
            Debug.log "ERROR" error
    in
    Cmd.none



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model.appState of
        Ready sharedState routerModel ->
            Router.view RouterMsg sharedState routerModel

        NotReady _ _ _ ->
            { title = "jHipster Elm Demo - Loading"
            , body = [ Html.text "Loading" ]
            }

        FailedToInitialize ->
            { title = "jHipster Elm Demo - Failure"
            , body = [ Html.text "The application failed to initialize. " ]
            }
