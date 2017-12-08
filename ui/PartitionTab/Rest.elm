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
        |> required "name" string
        |> required "kname" string
        |> required "maj:min" string
        |> required "fstype" string
        |> required "mountpoint" string
        |> required "label" string
        |> required "uuid" string
        |> required "parttype" string
        |> required "partlabel" string


getDisks : Cmd Msg
getDisks =
    Http.send InitPartitions
        (Http.get "http://localhost:8081/disks" decodeDisks)
