module Modules.Login.I18n.Phrases exposing (Phrase(..))

import I18n exposing (Language(..))


type Phrase
    = LoginTitle
    | UsernameLabel
    | UsernamePlaceholder
    | PasswordLabel
    | PasswordPlaceholder
    | RememberMeLabel
    | SignInButtonLabel
    | SignInLoadingLabel
    | ForgetPassword
    | NoAccountYet
    | RegisterNewAccount
    | WrongPasswordOrEmail
    | EmailNotConfirmed
    | FailedLogin
    | LoggedIn
    | LoggedInAs String
    | LogoutTitle
    -- General messages
    | Success
    | Error
    | ServerError
