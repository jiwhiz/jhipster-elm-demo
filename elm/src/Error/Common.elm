module Error.Common exposing (Context, UiElement, toContext, tt)

import Error.I18n.Phrases as ErrorPhrases
import Error.I18n.Translator exposing (translator)
import Shared.I18n exposing (Language(..))
import Shared.SharedState exposing (SharedState)
import UiFramework


type alias Context =
    { language : Language
    , translate : ErrorPhrases.Phrase -> String
    }


type alias UiElement msg =
    UiFramework.WithContext Context msg


toContext : SharedState -> UiFramework.UiContextual Context
toContext sharedState =
    { language = sharedState.language
    , translate = translator sharedState.language
    , device = sharedState.device
    , themeConfig = sharedState.themeConfig
    , parentRole = Nothing
    }


tt : ErrorPhrases.Phrase -> UiElement msg
tt phrase =
    UiFramework.uiContextualText
        (\context -> context.translate phrase)
