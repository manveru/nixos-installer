module TimezoneTab.Rest exposing (getTimezones)

import App.Types exposing (..)
import Http
import Json.Decode exposing (Decoder, at, list, map4, string)
import Json.Decode.Pipeline exposing (decode, optional, required)


decodeTimezones : Decoder (List Timezone)
decodeTimezones =
    Json.Decode.list
        (decode Timezone
            |> required "country" string
            |> required "coords" string
            |> required "region" string
            |> required "city" string
            |> required "name" string
            |> optional "comment" string ""
        )


getTimezones : Cmd Msg
getTimezones =
    Http.send InitTimezones
        (Http.get "/timezones" decodeTimezones)
