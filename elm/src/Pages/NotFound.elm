module Pages.NotFound exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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


view : Model -> ( String, Element Msg )
view model =
    ( " 404 Not Found"
    , el 
        [ height fill, centerX, paddingXY 10 10, Font.center ]
        ( h1 [] <| text "Page Not Found!")
    )


subscriptions : Model -> Sub Msg 
subscriptions model =
    Sub.none
