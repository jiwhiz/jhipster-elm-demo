module I18n.Languages.English exposing (translate)

import I18n.Phrases exposing(Phrase(..))

translate : Phrase -> String
translate phrase =
    case phrase of
        HomeTitle ->
            "Welcome, Java Hipster!"

        HomeSubtitle ->
            "This is your homepage"

        HomeLoggedin username ->
            "You are logged in as user [" ++ username ++ "]."

        HomeQuestion ->
            "If you have any question on JHipster:"

        HomeLike ->
            "If you like JHipster, don't forget to give us a star on"