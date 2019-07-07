module I18n.Languages.French exposing (translate)

import I18n.Phrases exposing(Phrase(..))

translate : Phrase -> String
translate phrase =
    case phrase of
        HomeTitle ->
            "Bienvenue, Java Hipster !"

        HomeSubtitle ->
            "Ceci est votre page d'accueil"

        HomeLoggedin username ->
            "Vous êtes connecté en tant que [" ++ username ++ "]."

        HomeQuestion ->
            "Si vous avez des questions à propos de JHipster:"

        HomeLike ->
            "Si vous aimez JHipster, donnez nous une étoile sur"