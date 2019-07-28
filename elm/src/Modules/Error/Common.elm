module Modules.Error.Common exposing (Context, UiElement, toContext, tt)

import Modules.Error.I18n.Phrases as ErrorPhrases
import Modules.Error.I18n.Translator exposing (translator)
import Modules.Shared.I18n exposing (Language(..))
import Modules.Shared.SharedState exposing (SharedState)
import UiFramework exposing (UiContextual, WithContext, uiText)


type alias Context =
    { language : Language
    , translate : ErrorPhrases.Phrase -> String
    }


type alias UiElement msg =
    WithContext Context msg


toContext : SharedState -> UiContextual Context
toContext sharedState =
    { language = sharedState.language
    , translate = translator sharedState.language
    , device = sharedState.device
    , themeConfig = sharedState.themeConfig
    , parentRole = Nothing
    }


tt : ErrorPhrases.Phrase -> UiElement msg
tt phrase =
    uiText
        (\context -> context.translate phrase)
