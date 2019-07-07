module Pages.Home exposing (..)

import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import I18n.I18n as I18n exposing (Translator)
import I18n.Phrases as Phrases
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import UiFramework.Alert as Alert
import UiFramework.Types exposing (Role(..), ScreenSize(..))


type alias Model = {}


type Msg 
    = NavigateTo Route
    | NoOp

init : ( Model, Cmd Msg )
init = 
    ( {}, Cmd.none )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (routeToUrlString route), NoUpdate )

        NoOp ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    let
        translate =
            I18n.translate sharedState.language
    in
    ( "Welcome"
    , el 
        [ height fill
        , width fill
        , centerX
        ]
        <| row [ width fill, height fill ]
            [ column
                [ width <| fillPortion 3
                , height fill
                , paddingXY 15 25
                , spacing 20
                ]
                [ el
                    [ width fill
                    , Font.alignLeft
                    , Font.size 28
                    , Font.color (rgb255 0 0 0)
                    , Font.medium
                    ]
                    ( text <| translate Phrases.HomeTitle )
                , el
                    [ width fill
                    , Font.alignLeft
                    , Font.size 18
                    , Font.color (rgb255 59 59 59)
                    , Font.light
                    ]
                    ( text <| translate Phrases.HomeSubtitle )
                , accountInfo sharedState
                ]
            , column 
                [ width <| fillPortion 1
                , alignTop
                ]
                [ el [ width fill, height fill ]
                    ( Html.img [Html.Attributes.src "/images/jhipster_family_member_2.svg"] []
                        |> Element.html
                    )
                ]
            ]

    )


accountInfo sharedState =
    let
        translate =
            I18n.translate sharedState.language
    in
    case sharedState.user of
        Just user ->
            Alert.simple Success <|
                ( text <| translate <| Phrases.HomeLoggedin user.username )

        Nothing ->
            Alert.simple Warning <|
                paragraph
                    [ Font.alignLeft
                    ]
                    [ text "Please "
                    , Alert.link Warning
                        { onPress = Just <| NavigateTo Login
                        , label = text "sign in"
                        }
                    , text "!"
                    ]
