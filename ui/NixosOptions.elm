module NixosOptions exposing (..)

import Html exposing (dd, div, dl, dt, p, text)
import Http
import InstallerTranslation exposing (t)
import Json.Decode exposing (Decoder, at, list, map4, string)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Material.Grid exposing (Device(..), cell, grid, size)
import Material.Options as Options
import Material.Typography as Typo
import Style exposing (fullWidth)
import T
import Translator exposing (Translator)


type alias NixosOption =
    { loc : List String
    , default : String
    , description : String
    , files : List String
    , type_ : String
    }


type Msg
    = NixosOptions (Result Http.Error NixosOption)


decodeOptions : Decoder NixosOption
decodeOptions =
    decode NixosOption
        |> required "loc" (list string)
        |> optional "default" string "no description"
        |> required "description" string
        |> required "files" (list string)
        |> required "type" string


getNixosOption : String -> Cmd Msg
getNixosOption name =
    Http.send NixosOptions
        (Http.get
            ("http://localhost:8081/option/" ++ name)
            decodeOptions
        )


getInitialNixosOptions : Cmd Msg
getInitialNixosOptions =
    getNixosOption "options"


nixosOptionsView :
    { a | nixosOptions : List String, translator : Translator.Translator }
    -> List (Material.Grid.Cell msg)
nixosOptionsView model =
    [ cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ t model T.NixosOptionsTitle ] ]
    , cell [ fullWidth ]
        [ Options.styled p [ Typo.subhead ] [ t model T.NixosOptionsDescription ] ]
    , cell [ fullWidth ]
        (List.map (\o -> div [] [ text o ]) model.nixosOptions)
    ]
