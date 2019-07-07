module UiFramework.Button exposing
    ( Button
    , default
    , simple
    , view
    , withBlock
    , withDisabled
    , withIcon
    , withLabel
    , withLarge
    , withMessage
    , withOutlined
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
import UiFramework.Icon as Icon
import UiFramework.Types exposing (Role(..), ScreenSize(..))



{- Button type -}


type Button msg
    = Button (Options msg)


type alias Options msg =
    { role : Role
    , outlined : Bool
    , block : Bool
    , disabled : Bool
    , size : ScreenSize
    , onPress : Maybe msg
    , icon : Maybe Icon.Icon
    , label : String
    , attributes : List (Attribute msg)
    }


withRole : Role -> Button msg -> Button msg
withRole role (Button options) =
    Button { options | role = role }


withOutlined : Button msg -> Button msg
withOutlined (Button options) =
    Button { options | outlined = True }


withBlock : Button msg -> Button msg
withBlock (Button options) =
    Button { options | block = True }


withDisabled : Button msg -> Button msg
withDisabled (Button options) =
    Button { options | disabled = True }


withLarge : Button msg -> Button msg
withLarge (Button options) =
    Button { options | size = LG }


withSmall : Button msg -> Button msg
withSmall (Button options) =
    Button { options | size = SM }


withMessage : Maybe msg -> Button msg -> Button msg
withMessage msg (Button options) =
    Button { options | onPress = msg }


withIcon : Icon.Icon -> Button msg -> Button msg
withIcon icon (Button options) =
    Button { options | icon = Just icon }


withLabel : String -> Button msg -> Button msg
withLabel label (Button options) =
    Button { options | label = label }


withExtraAttrs : List (Attribute msg) -> Button msg -> Button msg
withExtraAttrs attributes (Button options) =
    Button { options | attributes = attributes }


defaultOptions : Options msg
defaultOptions =
    { role = Primary
    , outlined = False
    , block = False
    , disabled = False
    , size = MD
    , onPress = Nothing
    , icon = Nothing
    , label = ""
    , attributes = []
    }


default : Button msg
default =
    Button defaultOptions


simple : msg -> String -> Button msg
simple msg label =
    default
        |> withMessage (Just msg)
        |> withLabel label



-- Rendering the button


view : Button msg -> Element msg
view (Button options) =
    Input.button
        (viewAttributes options)
        { onPress = options.onPress
        , label =
            case options.icon of
                Nothing ->
                    text options.label

                Just icon ->
                    row [ spacing 5 ] [ el [] <| Icon.view icon, el [] (text options.label) ]
        }


viewAttributes : Options mag -> List (Attribute msg)
viewAttributes options =
    let
        backgroundColor =
            defaultThemeColor options.role

        borderColor =
            defaultThemeColor options.role

        fontColor =
            contrastTextColor backgroundColor defaultTextDark defaultTextLight

        fontSize =
            16
    in
    [ paddingXY 16 10
    , Font.center
    , Font.size 16
    , Font.color fontColor
    , Border.rounded 4
    , Border.width 1
    , Border.solid
    , Border.color borderColor
    , Background.color backgroundColor
    ]
