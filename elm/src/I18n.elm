module I18n exposing (Language(..), Translator, languageFromCode, languageName)


type Language
    = English
    | French
    | ChineseSimplified


type alias Translator phase =
    phase -> String


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
            English


languageName : Language -> String
languageName lang =
    case lang of
        ChineseSimplified ->
            "中文（简体）"

        English ->
            "English"

        French ->
            "Français"
