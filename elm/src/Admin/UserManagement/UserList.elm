module Admin.UserManagement.UserList exposing (Model, Msg(..), init, update, view)

import Admin.UserManagement.Api.Request exposing (loadUsers)
import Admin.UserManagement.Api.User exposing (UserDTO)
import Admin.UserManagement.I18n.Phrases as UMPhrases
import Admin.UserManagement.I18n.Translator exposing (translator)
import Browser.Navigation exposing (pushUrl)
import DateFormat
import Element exposing (..)
import Element.Font as Font
import Html
import Html.Attributes
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import Shared.Api.User exposing (User)
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..))
import Time
import Toasty.Defaults
import UiFramework exposing (UiContextual, WithContext, flatMap, fromElement, toElement, uiColumn, uiParagraph, uiRow, uiText)
import UiFramework.Alert as Alert
import UiFramework.Internal as Internal
import UiFramework.Table as Table
import UiFramework.Types exposing (Role(..))
import UiFramework.Typography exposing (h1, textLead)


type alias Model =
    { users : List UserDTO
    , state : DataPageState
    }


type DataPageState
    = Loading
    | Loaded (List UserDTO) -- add pagination
    | Failed String


type alias Context =
    { translate : UMPhrases.Phrase -> String
    , admin : Maybe User
    }


type alias UiElement msg =
    WithContext Context msg


type Msg
    = NavigateTo Route
    | LoadUsersResponse (RemoteData.WebData (List UserDTO))


init : Maybe String -> ( Model, Cmd Msg )
init token =
    ( { users = []
      , state = Loading
      }
    , loadUsers token LoadUsersResponse
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (routeToUrlString route), NoUpdate )

        LoadUsersResponse (RemoteData.Failure e) ->
            ( { model | state = Failed "Load users error" }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error "Error" "error"
            )

        LoadUsersResponse (RemoteData.Success users) ->
            ( { model | state = Loaded users }
            , Cmd.none
            , NoUpdate
            )

        LoadUsersResponse _ ->
            ( model, Cmd.none, NoUpdate )


toContext : SharedState -> UiContextual Context
toContext sharedState =
    { translate = translator sharedState.language
    , admin = sharedState.user
    , device = sharedState.device
    , themeConfig = sharedState.themeConfig
    , parentRole = Nothing
    }


tt : UMPhrases.Phrase -> UiElement Msg
tt phrase =
    uiText
        (\context -> context.translate phrase)


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Users"
    , case model.state of
        Loading ->
            text "Loading"

        Loaded users ->
            toElement (toContext sharedState) (content model)

        Failed e ->
            text e
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
            tt <|
                UMPhrases.UserListTitle
        , case model.state of
            Loading ->
                fromElement (\_ -> text "Loading")

            Loaded users ->
                userTable users

            Failed e ->
                fromElement (\_ -> text e)
        ]


userTable : List UserDTO -> UiElement Msg
userTable users =
    Table.simpleTable
        --|> Table.withCompact
        |> Table.withBordered
        |> Table.withColumns
            [ createColumn UMPhrases.Id (.id >> String.fromInt)
            , createColumn UMPhrases.FirstName (.firstName >> Maybe.withDefault "")
            , createColumn UMPhrases.LastName (.lastName >> Maybe.withDefault "")
            , createColumn UMPhrases.Email .email
            , createColumn UMPhrases.Language .languageKey
            , createColumn UMPhrases.CreatedBy .createdBy
            , createColumn UMPhrases.CreatedDate (.createdDate >> formatDate)
            , createColumn UMPhrases.LastModifiedBy .lastModifiedBy
            , createColumn UMPhrases.LastModifiedDate (.lastModifiedDate >> formatDate)
            ]
        |> Table.view users


createColumn : UMPhrases.Phrase -> (UserDTO -> String) -> Table.Column UserDTO (UiContextual Context) Msg
createColumn headPhrase f =
    { head = tt headPhrase
    , viewData =
        \user ->
            Internal.fromElement
                (\_ -> Element.text (f user))
    }


formatDate : Maybe Time.Posix -> String
formatDate maybeDate =
    case maybeDate of
        Nothing ->
            ""

        Just t ->
            DateFormat.format
                [ DateFormat.yearNumber
                , DateFormat.text "-"
                , DateFormat.monthFixed
                , DateFormat.text "-"
                , DateFormat.dayOfMonthNumber
                , DateFormat.text " "
                , DateFormat.hourMilitaryFixed
                , DateFormat.text ":"
                , DateFormat.minuteFixed
                ]
                Time.utc
                t
