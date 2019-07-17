module Modules.Home.I18n.Translator exposing (translator)

import I18n exposing (Language(..), Translator)
import Modules.Home.I18n.ChineseSimplified as ChineseSimplified
import Modules.Home.I18n.English as English
import Modules.Home.I18n.French as French
import Modules.Home.I18n.Phrases exposing(Phrase)


translator : Language -> Translator Phrase
translator lang =
    case lang of
        ChineseSimplified ->
            ChineseSimplified.translate

        English ->
            English.translate

        French ->
            French.translate