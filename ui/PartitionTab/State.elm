module PartitionTab.State exposing (..)

import App.Types exposing (..)
import PartitionTab.Rest as Rest


init : ( Model, Cmd Msg )
init =
    ( { disk = nullDisk
      , disks = []
      }
    , Cmd.batch [ Rest.getDisks ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Init (Ok newDisks) ->
            ( { model | disks = newDisks }, Cmd.none )

        Init (Err _) ->
            ( model, Cmd.none )

        Choose diskPath ->
            ( { model | disk = findDisk model diskPath }, Cmd.none )


findDisk : Model -> String -> Disk
findDisk model diskPath =
    Maybe.withDefault nullDisk
        (List.head (List.filter (\disk -> disk.path == diskPath) model.disks))


diskAttribute : Maybe Disk -> (Disk -> a) -> a
diskAttribute disk extractor =
    disk |> Maybe.withDefault nullDisk |> extractor
