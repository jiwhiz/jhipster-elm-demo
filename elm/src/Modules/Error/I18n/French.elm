module Modules.Error.I18n.French exposing (translate)

import Modules.Error.I18n.Phrases exposing(Phrase(..))

translate : Phrase -> String
translate phrase =
    case phrase of
        NotFoundTitle ->
            "La page n'existe pas."
