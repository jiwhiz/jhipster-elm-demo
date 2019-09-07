module Home.Home exposing (Model, Msg(..), init, update, view)

import Browser.Navigation exposing (pushUrl)
import Element exposing (DeviceClass(..), Element, Orientation(..), alignTop, el, fill, fillPortion, height, link, paddingXY, spacing, text, width)
import Element.Font as Font
import Home.I18n.Phrases as HomePhrases
import Home.I18n.Translator exposing (translator)
import Html
import Html.Attributes
import Routes exposing (Route(..), routeToUrlString)
import Shared.Api.User exposing (User)
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..))
import UiFramework
import UiFramework.Alert as Alert
import UiFramework.Typography exposing (h1, textLead)


type alias Model =
    {}


type alias Context =
    { translate : HomePhrases.Phrase -> String
    , user : Maybe User
    }


type alias UiElement msg =
    UiFramework.WithContext Context msg


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


toContext : SharedState -> UiFramework.UiContextual Context
toContext sharedState =
    { translate = translator sharedState.language
    , user = sharedState.user
    , device = sharedState.device
    , themeConfig = sharedState.themeConfig
    , parentRole = Nothing
    }


tt : HomePhrases.Phrase -> UiElement Msg
tt phrase =
    UiFramework.uiContextualText
        (\context -> context.translate phrase)


view : SharedState -> Model -> ( String, Element Msg )
view sharedState _ =
    ( "Welcome"
    , UiFramework.toElement (toContext sharedState) content
    )


content : UiElement Msg
content =
    UiFramework.withContext
        (\context ->
            let
                homeInfo =
                    [ h1 [ paddingXY 0 30 ] <| tt HomePhrases.Title
                    , textLead [] <| tt HomePhrases.Subtitle
                    , case context.user of
                        Just user ->
                            Alert.simpleSuccess <|
                                (tt <| HomePhrases.LoggedInAs user.username)

                        Nothing ->
                            Alert.simpleWarning <|
                                UiFramework.uiColumn
                                    [ spacing 20 ]
                                    [ UiFramework.uiParagraph
                                        [ Font.alignLeft
                                        ]
                                        [ tt HomePhrases.SignInPrefix
                                        , Alert.link
                                            { onPress = Just <| NavigateTo Login
                                            , label = tt HomePhrases.SignInLink
                                            }
                                        , tt HomePhrases.SignInSuffix
                                        ]
                                    , tt HomePhrases.AdminAccountInfo
                                    , tt HomePhrases.UserAccountInfo
                                    ]
                    , UiFramework.uiParagraph
                        [ Font.alignLeft ]
                        [ tt HomePhrases.Like
                        , UiFramework.uiLink
                            { url = "https://github.com/jiwhiz/jhipster-elm-demo"
                            , label = " Github!"
                            }
                        ]
                    ]

                hipsterImg =
                    UiFramework.fromElement
                        (\_ ->
                            el
                                [ width fill
                                , height fill
                                ]
                                (Html.img [ Html.Attributes.src "/images/jhipster_family_member_2.svg" ] []
                                    |> Element.html
                                )
                        )
            in
            if context.device.class == Phone then
                UiFramework.uiColumn
                    [ width fill
                    , height fill
                    , paddingXY 5 10
                    , spacing 20
                    ]
                    (homeInfo ++ [ hipsterImg ])

            else
                UiFramework.uiRow
                    [ width fill, height fill ]
                    [ UiFramework.uiColumn
                        [ width <| fillPortion 3
                        , height fill
                        , paddingXY 20 30
                        , spacing 20
                        ]
                        homeInfo
                    , UiFramework.uiColumn
                        [ width <| fillPortion 1
                        , alignTop
                        ]
                        [ hipsterImg ]
                    ]
        )
