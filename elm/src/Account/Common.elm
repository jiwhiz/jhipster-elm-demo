module Account.Common exposing (Context, UiElement, sharedForms, toContext, tt)

import Account.I18n.Phrases as AccountPhrases
import Account.I18n.Translator exposing (translator)
import Shared.I18n exposing (Language, languageCode, languageName, supportLanguages)
import Shared.SharedState exposing (SharedState)
import UiFramework exposing (UiContextual, WithContext, uiContextualText)
import UiFramework.Form.ComposableForm as ComposableForm
import UiFramework.Form.SelectField as SelectField
import UiFramework.Form.TextField as TextField


type alias Context =
    { language : Language
    , translate : AccountPhrases.Phrase -> String
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


tt : AccountPhrases.Phrase -> UiElement msg
tt phrase =
    uiContextualText
        (\context -> context.translate phrase)


sharedForms :
    UiContextual Context
    ->
        { usernameField : ComposableForm.Form { r | username : String } String
        , firstNameField : ComposableForm.Form { r | firstName : String } String
        , lastNameField : ComposableForm.Form { r | lastName : String } String
        , emailField : ComposableForm.Form { r | email : String } String
        , currentPasswordField : ComposableForm.Form { r | currentPassword : String } String
        , passwordField : ComposableForm.Form { r | password : String } String
        , repeatPasswordField : ComposableForm.Form { r | password : String, repeatPassword : String } ()
        , languageField : ComposableForm.Form { r | languageKey : String } String
        }
sharedForms context =
    let
        translate =
            context.translate
    in
    { usernameField =
        ComposableForm.textField
            { parser = Ok
            , value = .username
            , update = \value values -> { values | username = value }
            , error = always Nothing
            , attributes =
                TextField.defaultAttributes
                    |> TextField.withLabel (translate AccountPhrases.UsernameLabel)
                    |> TextField.withPlaceholder (translate AccountPhrases.UsernamePlaceholder)
            }
    , firstNameField =
        ComposableForm.textField
            { parser = Ok
            , value = .firstName
            , update = \value values -> { values | firstName = value }
            , error = always Nothing
            , attributes =
                TextField.defaultAttributes
                    |> TextField.withLabel (translate AccountPhrases.FirstnameLabel)
                    |> TextField.withPlaceholder (translate AccountPhrases.FirstnamePlaceholder)
            }
    , lastNameField =
        ComposableForm.textField
            { parser = Ok
            , value = .lastName
            , update = \value values -> { values | lastName = value }
            , error = always Nothing
            , attributes =
                TextField.defaultAttributes
                    |> TextField.withLabel (translate AccountPhrases.LastnameLabel)
                    |> TextField.withPlaceholder (translate AccountPhrases.LastnamePlaceholder)
            }
    , emailField =
        ComposableForm.textField
            { parser = Ok
            , value = .email
            , update = \value values -> { values | email = value }
            , error = always Nothing
            , attributes =
                TextField.defaultAttributes
                    |> TextField.withLabel (translate AccountPhrases.EmailLabel)
                    |> TextField.withPlaceholder (translate AccountPhrases.EmailPlaceholder)
            }
    , currentPasswordField =
        ComposableForm.passwordField
            { parser = Ok
            , value = .currentPassword
            , update = \value values -> { values | currentPassword = value }
            , error = always Nothing
            , attributes =
                TextField.defaultAttributes
                    |> TextField.withLabel (translate AccountPhrases.CurrentPasswordLabel)
                    |> TextField.withPlaceholder (translate AccountPhrases.CurrentPasswordPlaceholder)
            }
    , passwordField =
        ComposableForm.passwordField
            { parser = Ok
            , value = .password
            , update = \value values -> { values | password = value }
            , error = always Nothing
            , attributes =
                TextField.defaultAttributes
                    |> TextField.withLabel (translate AccountPhrases.NewPasswordLabel)
                    |> TextField.withPlaceholder (translate AccountPhrases.NewPasswordPlaceholder)
            }
    , repeatPasswordField =
        ComposableForm.meta
            (\values ->
                ComposableForm.passwordField
                    { parser =
                        \value ->
                            if value == values.password then
                                Ok ()

                            else
                                Err <| translate AccountPhrases.PasswordNotMatch
                    , value = .repeatPassword
                    , update =
                        \newValue values_ ->
                            { values_ | repeatPassword = newValue }
                    , error = always Nothing
                    , attributes =
                        TextField.defaultAttributes
                            |> TextField.withLabel (translate AccountPhrases.ConfirmPasswordLabel)
                            |> TextField.withPlaceholder (translate AccountPhrases.ConfirmPasswordPlaceholder)
                    }
            )
    , languageField =
        ComposableForm.selectField
            { parser = Ok
            , value = .languageKey
            , update = \value values -> { values | languageKey = value }
            , error = always Nothing
            , attributes =
                SelectField.defaultAttributes
                    |> SelectField.withLabel (translate AccountPhrases.LanguageLabel)
                    |> SelectField.withOptions
                        (List.map
                            (\lang -> ( languageCode lang, languageName lang ))
                            supportLanguages
                        )
            }
    }
