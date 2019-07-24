module Modules.Shared.I18n.Translator exposing (translator)

import I18n exposing (Language(..), Translator)
import Modules.Shared.I18n.ChineseSimplified as ChineseSimplified
import Modules.Shared.I18n.English as English
import Modules.Shared.I18n.French as French
import Modules.Shared.I18n.Phrases exposing(Phrase)


translator : Language -> Translator Phrase
translator lang =
    case lang of
        ChineseSimplified ->
            ChineseSimplified.translate

        English ->
            English.translate

        French ->
            French.translate