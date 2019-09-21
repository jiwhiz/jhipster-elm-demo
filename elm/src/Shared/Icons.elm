module Shared.Icons exposing
    ( adminIcon
    , bootstrapIcon
    , flagIcon
    , homeIcon
    , loginIcon
    , logoutIcon
    , passwordIcon
    , registerIcon
    , settingsIcon
    , userIcon
    )

import FontAwesome.Brands
import FontAwesome.Solid
import FontAwesome.Styles
import UiFramework.Icon as Icon


adminIcon =
    Icon.fontAwesome FontAwesome.Solid.userPlus


bootstrapIcon =
    Icon.fontAwesome FontAwesome.Brands.bootstrap


flagIcon =
    Icon.fontAwesome FontAwesome.Solid.flag


homeIcon =
    Icon.fontAwesome FontAwesome.Solid.home


loginIcon =
    Icon.fontAwesome FontAwesome.Solid.signInAlt


logoutIcon =
    Icon.fontAwesome FontAwesome.Solid.signOutAlt


passwordIcon =
    Icon.fontAwesome FontAwesome.Solid.key


registerIcon =
    Icon.fontAwesome FontAwesome.Solid.cashRegister


settingsIcon =
    Icon.fontAwesome FontAwesome.Solid.wrench


userIcon =
    Icon.fontAwesome FontAwesome.Solid.user
