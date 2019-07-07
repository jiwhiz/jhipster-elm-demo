module UiFramework.Alert exposing
    ( Alert
    , Options
    , default
    , link
    , simple
    , view
    , withChild
    , withExtraAttrs
    , withLarge
    , withRole
    , withSmall
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import UiFramework.Colors exposing (..)
import UiFramework.Types exposing (Role(..), ScreenSize(..))


type Alert msg
    = Alert (Options msg)


type alias Options msg =
    { role : Role
    , size : ScreenSize
    , attributes : List (Attribute msg)
    , child : Element msg
    }


withRole : Role -> Alert msg -> Alert msg
withRole role (Alert options) =
    Alert { options | role = role }


withLarge : Alert msg -> Alert msg
withLarge (Alert options) =
    Alert { options | size = LG }


withSmall : Alert msg -> Alert msg
withSmall (Alert options) =
    Alert { options | size = SM }


withExtraAttrs : List (Attribute msg) -> Alert msg -> Alert msg
withExtraAttrs attributes (Alert options) =
    Alert { options | attributes = attributes }


withChild : Element msg -> Alert msg -> Alert msg
withChild child (Alert options) =
    Alert { options | child = child }


defaultOptions : Options msg
defaultOptions =
    { role = Primary
    , size = MD
    , attributes = []
    , child = none
    }


default : Alert msg
default =
    Alert defaultOptions


simple : Role -> Element msg -> Element msg
simple role child =
    default
        |> withRole role
        |> withChild child
        |> view


link :
    Role
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
link role { onPress, label } =
    let
        fontColor =
            defaultAlertLinkFontColor role
    in
    Input.button
        [ Font.bold, Font.color fontColor ]
        { onPress = onPress
        , label = label
        }



-- Rendering Alert


view : Alert msg -> Element msg
view (Alert options) =
    el
        (viewAttributes options)
        options.child


viewAttributes : Options mag -> List (Attribute msg)
viewAttributes options =
    let
        backgroundColor =
            defaultAlertBackgroundColor options.role

        borderColor =
            defaultAlertBorderColor options.role

        fontColor =
            defaultAlertFontColor options.role

        fontSize =
            16
    in
    [ width fill
    , paddingXY 20 16
    , Font.alignLeft
    , Font.size 16
    , Font.color fontColor
    , Border.rounded 4
    , Border.width 1
    , Border.solid
    , Border.color borderColor
    , Background.color backgroundColor
    ]
