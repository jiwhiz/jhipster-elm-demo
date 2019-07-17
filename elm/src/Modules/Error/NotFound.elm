module Modules.Error.NotFound exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Modules.Error.I18n.Phrases as ErrorPhrases
import Modules.Error.I18n.Translator exposing(translator)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import UiFramework.Typography exposing(h1)


type alias Model = {}


type Msg 
    = NoOp


init : ( Model, Cmd Msg )
init = 
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    let
        translate =
            translator sharedState.language
    in
    ( " 404 Not Found"
    , el 
        [ height fill, centerX, paddingXY 10 10, Font.center ]
        ( h1 [] <| text <| translate ErrorPhrases.NotFoundTitle )
    )


subscriptions : Model -> Sub Msg 
subscriptions model =
    Sub.none
