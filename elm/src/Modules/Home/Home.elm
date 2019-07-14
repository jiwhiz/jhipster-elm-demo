module Modules.Home.Home exposing (Model, Msg(..), accountInfo, init, update, view)

import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Font as Font
import Html
import Html.Attributes
import Modules.Home.I18n.Phrases as HomePhrases
import Modules.Home.I18n.Translator exposing (translator)
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import UiFramework.Alert as Alert
import UiFramework.Types exposing (Role(..), ScreenSize(..))
import UiFramework.Typography exposing (h1, textLead)


type alias Model =
    {}


type Msg
    = NavigateTo Route


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (routeToUrlString route), NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    let
        translate =
            translator sharedState.language
    in
    ( "Welcome"
    , el
        [ height fill
        , width fill
        , centerX
        ]
      <|
        row [ width fill, height fill ]
            [ column
                [ width <| fillPortion 3
                , height fill
                , paddingXY 15 25
                , spacing 20
                ]
                [ h1 []
                    (text <| translate HomePhrases.Title)
                , textLead []
                    (text <| translate HomePhrases.Subtitle)
                , accountInfo sharedState
                , paragraph
                    [ Font.alignLeft ]
                    [ text <| translate HomePhrases.Like
                    , text " "
                    , link []
                        { url = "https://github.com/jhipster/generator-jhipster"
                        , label = text "Github"
                        }
                    ]
                ]
            , column
                [ width <| fillPortion 1
                , alignTop
                ]
                [ el [ width fill, height fill ]
                    (Html.img [ Html.Attributes.src "/images/jhipster_family_member_2.svg" ] []
                        |> Element.html
                    )
                ]
            ]
    )


accountInfo sharedState =
    let
        translate =
            translator sharedState.language
    in
    case sharedState.user of
        Just user ->
            Alert.simple Success <|
                (text <| translate <| HomePhrases.LoggedInAs user.username)

        Nothing ->
            Alert.simple Warning <|
                column
                    [ spacing 10 ]
                    [ paragraph
                        [ Font.alignLeft
                        ]
                        [ text <| translate <| HomePhrases.SignInPrefix
                        , Alert.link Warning
                            { onPress = Just <| NavigateTo Login
                            , label = text <| translate <| HomePhrases.SignInLink
                            }
                        , text <| translate <| HomePhrases.SignInSuffix
                        ]
                    , text <| translate <| HomePhrases.AdminAccountInfo
                    , text <| translate <| HomePhrases.UserAccountInfo
                    ]
