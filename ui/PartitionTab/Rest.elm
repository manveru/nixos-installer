module PartitionTab.Rest exposing (getDisks)

import App.Types exposing (..)
import Http
import Json.Decode exposing (Decoder, at, list, map4, string)
import Json.Decode.Pipeline exposing (decode, optional, required)


decodeDisks : Decoder (List Disk)
decodeDisks =
    Json.Decode.list decodeDisk


decodeDisk : Decoder Disk
decodeDisk =
    decode Disk
        |> required "path" string
        |> required "model" string
        |> required "serial" string
        |> required "size" string


getDisks : Cmd Msg
getDisks =
    Http.send InitPartitions
        (Http.get "http://localhost:8081/disks" decodeDisks)
