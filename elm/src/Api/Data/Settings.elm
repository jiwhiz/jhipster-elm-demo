module Api.Data.Settings exposing (Settings, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)

type alias Settings =
    { firstName : String
    , lastName : String
    , email : String
    -- , language : String
    }

decoder : Decoder Settings
decoder =
    Decode.succeed Settings
        |> required "firstName" Decode.string
        |> required "lastName" Decode.string
        |> required "email" Decode.string
        -- |> required "langKey" Decode.string


encoder : Settings -> Encode.Value
encoder model =
    Encode.object
        [ ( "firstName", Encode.string model.firstName )
        , ( "lastName", Encode.string model.lastName )
        , ( "email", Encode.string model.email )
        -- , ( "langKey", Encode.string model.language )
        ]