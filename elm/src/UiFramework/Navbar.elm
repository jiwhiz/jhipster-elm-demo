module UiFramework.Navbar exposing
    ( .. )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import FontAwesome.Icon
import Html exposing (Html)
import Html.Events
import Json.Decode as Json
import UiFramework.Colors as Colors
import UiFramework.Icon as Icon
import UiFramework.Types exposing (Role(..), ScreenSize(..))


type Navbar msg state
    = Navbar (NavbarOptions msg state)


type alias NavbarOptions msg state =
    { toggleMenuMsg : msg
    , brand : Maybe (Element msg)
    , backgroundColor : BackgroundColor
    , items : List (MenuItem msg state)
    , attributes : List (Attribute msg)
    }


type alias NavbarState state =
    { deviceClass : DeviceClass
    , toggleMenuState : Bool
    , dropdownState : state
    }


type BackgroundColor
    = Roled Role
    | Custom Color
    | Class String


type MenuItem msg state
    = LinkItem (LinkItemOptions msg)
    | DropdownItem (Dropdown msg state)
    | CustomItem


type alias LinkItemOptions msg =
    { triggerMsg : msg
    , icon : Maybe Icon.Icon
    , title : String
    }


type Dropdown msg state
    = Dropdown (DropdownOptions msg state)


type DropdownMenuItem msg
    = DropdownMenuItem (LinkItemOptions msg)


type alias DropdownOptions msg state =
    { toggleDropdownMsg : msg
    , openState : state
    , icon : Maybe Icon.Icon
    , title : String
    , items : List (DropdownMenuItem msg)
    }


withBrand : Element msg -> Navbar msg state -> Navbar msg state
withBrand brand (Navbar options) =
    Navbar {  options | brand = Just brand }


withBackground : Role -> Navbar msg state -> Navbar msg state
withBackground role  (Navbar options)=
    Navbar {  options | backgroundColor = Roled role }


withBackgroundColor : Color -> Navbar msg state -> Navbar msg state
withBackgroundColor color  (Navbar options)=
    Navbar {  options | backgroundColor = Custom color }


withMenuItems : List (MenuItem msg state)-> Navbar msg state -> Navbar msg state
withMenuItems items (Navbar options)=
    Navbar {  options | items = items }


withExtraAttrs : List (Attribute msg) ->  Navbar msg state -> Navbar msg state
withExtraAttrs attributes (Navbar options) =
    Navbar { options | attributes = attributes }


default : msg -> Navbar msg state
default msg =
    Navbar 
        { toggleMenuMsg = msg
        , brand = Nothing
        , backgroundColor = Roled Light
        , items = []
        , attributes = []
    }


linkItem : msg -> MenuItem msg state
linkItem msg =
    LinkItem
        { triggerMsg = msg
        , icon = Nothing
        , title = ""
        }


withMenuIcon : Icon.Icon -> MenuItem msg state -> MenuItem msg state
withMenuIcon icon item =
    case item of
        LinkItem options ->
            LinkItem { options | icon = Just icon }
        DropdownItem (Dropdown options) ->
            DropdownItem <| Dropdown { options | icon = Just icon }
        CustomItem ->
            item


withMenuTitle : String -> MenuItem msg state -> MenuItem msg state
withMenuTitle title item =
    case item of
        LinkItem options ->
            LinkItem { options | title = title }
        DropdownItem (Dropdown options) ->
            DropdownItem <| Dropdown { options | title = title }
        CustomItem ->
            item


dropdown : msg -> state -> Dropdown msg state
dropdown msg openState =
    Dropdown
        { toggleDropdownMsg = msg
        , openState = openState
        , icon = Nothing
        , title = ""
        , items = []
        }


withDropdownMenuItems : List (DropdownMenuItem msg) -> Dropdown msg state -> Dropdown msg state
withDropdownMenuItems items (Dropdown options) =
    Dropdown { options | items = items }


dropdownMenuItem : msg -> DropdownMenuItem msg
dropdownMenuItem msg =
    DropdownMenuItem
        { triggerMsg = msg
        , icon = Nothing
        , title = ""
        }


withDropdownMenuIcon : Icon.Icon -> DropdownMenuItem msg -> DropdownMenuItem msg
withDropdownMenuIcon icon (DropdownMenuItem options) =
    DropdownMenuItem { options | icon = Just icon}


withDropdownMenuTitle : String -> DropdownMenuItem msg -> DropdownMenuItem msg
withDropdownMenuTitle title (DropdownMenuItem options) =
    DropdownMenuItem { options | title = title }


-- Render Navbar

view : NavbarState state -> Navbar msg state -> Element msg
view state (Navbar options) =
    let
        backgroundColor =
            case options.backgroundColor of
                Roled role ->
                    Colors.defaultThemeColor role
                Custom color ->
                    color
                Class cssStr ->
                    Colors.getColor cssStr

        fontColor = 
            Colors.contrastTextColor backgroundColor Colors.defaultTextDark Colors.defaultTextLight
    
        headerAttrs =
            [ width fill
            , paddingXY 20 16
            , Border.solid
            , Border.color Colors.gray300
            , Border.widthEach {bottom=1, top=0, left=0, right=0}
            , Background.color backgroundColor
            , Font.color fontColor
            ]

        brand =
            options.brand |> Maybe.withDefault none

        navButton : msg -> Element msg
        navButton toggleMenuMsg =
            el 
                [ onClick toggleMenuMsg
                , alignRight
                , Border.color fontColor
                , Border.solid
                , Border.width 1
                , Border.rounded 3
                , Border.shadow { offset = (0, 0), size = 0, blur = 4, color = rgba 0 0 0 0.2} 
                ] <|
                    column [ spacing 4, padding 10 ]
                        [ iconBar, iconBar, iconBar ]

        iconBar =
            el 
                [ width <| px 22
                , height <| px 2
                , Background.color fontColor
                , Border.rounded 1
                ] none
    in
    case state.deviceClass of
        Phone ->
            column headerAttrs
                <| [ row [ width fill, paddingEach { top = 10, right = 20, bottom = 10, left = 10 } ] 
                        [ brand, navButton options.toggleMenuMsg ] ] ++ 
                    (if state.toggleMenuState then [ viewCollapsedMenuList state options.items ] else [])
        _ ->
            row headerAttrs [ brand, viewMenubarList state options.items ]


viewCollapsedMenuList : NavbarState state -> List (MenuItem msg state) -> Element msg
viewCollapsedMenuList state items =
    column 
        [ Region.navigation
        , width fill
        , paddingXY 0 10
        , spacing 5
        , alignLeft
        , Font.size 14
        , Font.alignLeft
        ]
        <| List.map (viewMenuItem state) items


viewMenubarList : NavbarState state -> List (MenuItem msg state) -> Element msg
viewMenubarList state items =
    row [ Region.navigation
        , paddingEach { top = 30, right = 30, bottom = 10, left = 100}
        , spacing 15
        , alignRight
        , Font.size 16
        , Font.center
        , Font.medium
        ]
        <| List.map (viewMenuItem state) items


viewMenuItem : NavbarState state -> MenuItem msg state -> Element msg
viewMenuItem state item =
    case item of
        LinkItem options ->
            viewLinkItem options

        DropdownItem (Dropdown options) ->
            viewDropdownItem state options

        CustomItem ->
            none


viewLinkItem : LinkItemOptions msg -> Element msg
viewLinkItem options =
    el
        [ onClick options.triggerMsg
        , paddingEach { top = 8, right = 12, bottom = 8, left = 12}
        , width fill
        , pointer
        ] 
        (
            case options.icon of
                Nothing ->
                    text options.title

                Just icon ->
                    row [ spacing 5 ] [ el [] <| Icon.view icon, el [] (text options.title) ]
        )


viewDropdownItem : NavbarState state -> DropdownOptions msg state -> Element msg
viewDropdownItem state options =
    el
        [ onClick options.toggleDropdownMsg
        , paddingEach { top = 8, right = 12, bottom = 8, left = 12}
        , pointer
        , below <|
            if (state.dropdownState == options.openState) then
                viewDropdownMenu options.items
            else
                none
        ]
        (
            case options.icon of
                Nothing ->
                    text <| options.title ++ " ▾"

                Just icon ->
                    row [ spacing 5 ] [ el [] <| Icon.view icon, el [] (text <| options.title ++ " ▾") ]
        )


viewDropdownMenu : List (DropdownMenuItem msg) -> Element msg
viewDropdownMenu items =
    el [ alignRight ] <| 
        column 
            [ paddingXY 10 10
            , spacing 0
            , Background.color Colors.white
            , Font.color Colors.gray900
            , Font.alignLeft
            , Border.rounded 5
            , Border.color (rgba 0 0 0 0.15)
            , Border.solid
            , Border.width 1
            , Border.shadow { offset = (0, 5), size = 0, blur = 10, color = rgba 0 0 0 0.2}
            -- , Element.explain Debug.todo
            ]
            ( List.map 
                (\(DropdownMenuItem options) -> 
                    viewLinkItem options
                )
                items
            )


onClick : msg -> Attribute msg
onClick message =
    Html.Events.custom
        "click"
        ( Json.succeed 
            { message = message
            , stopPropagation = True
            , preventDefault = False
            }
        )
    |> htmlAttribute


