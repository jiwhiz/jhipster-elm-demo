module Modules.Error.I18n.English exposing (translate)

import Modules.Error.I18n.Phrases exposing(Phrase(..))

translate : Phrase -> String
translate phrase =
    case phrase of
        NotFoundTitle ->
            "The page does not exist."
