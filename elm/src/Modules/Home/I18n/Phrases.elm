module Modules.Home.I18n.Phrases exposing (Phrase(..))

import I18n exposing (Language(..))


type Phrase
    = Title
    | Subtitle
    | LoggedInAs String
    | SignInPrefix
    | SignInLink
    | SignInSuffix
    | AdminAccountInfo
    | UserAccountInfo
    | Question
    | Like
