module Pages.Logout exposing (..)

import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import LocalStorage exposing (jwtAuthenticationTokenKey)
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))


type alias Model =
    {}


type Msg 
    = NoOp

{- When logout, we clear jwt token in local storage. -}
init : ( Model, Cmd Msg )
init =
    ( {}, LocalStorage.clear jwtAuthenticationTokenKey )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )


view : Model -> ( String, Element Msg )
view model =
    ( "Logout"
    , el
        [ height fill
        , alignLeft
        , paddingXY 30 50
        , Font.size 28
        , Font.color (rgb255 59 59 59)
        , Font.medium
        ]
        (text "Logged out successfully!")
    )

