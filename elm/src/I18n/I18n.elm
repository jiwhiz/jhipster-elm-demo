module I18n.I18n exposing(Language(..), Translator, languageFromCode, languageName, translate)

import I18n.Languages.ChineseSimplified as ChineseSimplified
import I18n.Languages.English as English
import I18n.Languages.French as French
import I18n.Phrases exposing(Phrase)


type Language
    = English
    | French
    | ChineseSimplified


type alias Translator =
    Phrase -> String


translate : Language -> Translator
translate lang =
    case lang of
        ChineseSimplified ->
            ChineseSimplified.translate

        English ->
            English.translate

        French ->
            French.translate


languageFromCode : String -> Language
languageFromCode code =
    case code of
        "en" ->
            English

        "fr" ->
            French

        "zh-cn" ->
            ChineseSimplified

        _ ->
            English   -- default to English for not recoginzed code string


languageName : Language -> String
languageName lang =
    case lang of
        ChineseSimplified ->
            "中文（简体）"

        English ->
            "English"

        French ->
            "Français"

