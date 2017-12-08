module NixosOptions exposing (..)

import App.Types exposing (..)
import Dict
import Html exposing (dd, div, dl, dt, p, text)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Material.Grid exposing (Device(..), cell, grid, size)
import Material.Options as Options
import Material.Typography as Typo
import Style exposing (fullWidth)
import T exposing (t)
import Translator exposing (Translator)


decodeOption : Decoder ConfigOption
decodeOption =
    field "type" string
        |> andThen
            (\typeName ->
                case typeName of
                    "integer" ->
                        map IntConfig decodeInt

                    "null or integer" ->
                        map NullOrIntConfig decodeNullOrInt

                    "list of strings" ->
                        map ListOfStringsConfig decodeListOfStrings

                    "list of packages" ->
                        map ListOfStringsConfig decodeListOfStrings

                    "list or attribute set of submodules" ->
                        map UnspecifiedConfig decodeUnspecified

                    "list of attribute sets" ->
                        map ListOfStringsConfig decodeListOfStrings

                    "attribute set of paths" ->
                        map DictConfig decodeDict

                    "unspecified" ->
                        map UnspecifiedConfig decodeUnspecified

                    "boolean" ->
                        map BooleanConfig decodeBoolean

                    "package" ->
                        map StringConfig decodeString

                    "string" ->
                        map StringConfig decodeString

                    "null or string" ->
                        map StringConfig decodeString

                    "null or submodule" ->
                        map StringConfig decodeString

                    "path" ->
                        map StringConfig decodeString

                    "null or path" ->
                        map StringConfig decodeString

                    "attribute set of sysctl option values" ->
                        map StringConfig decodeString

                    _ ->
                        fail ("Can't decode a " ++ typeName)
            )


decodeString : Decoder { default : Maybe String, example : Maybe String }
decodeString =
    map2 (\default example -> { default = default, example = example })
        (maybe (field "default" string))
        (maybe (field "example" string))


decodeBoolean : Decoder { default : Bool, example : Maybe Bool }
decodeBoolean =
    map2 (\default example -> { default = default, example = example })
        (field "default" bool)
        (maybe (field "example" bool))


decodeUnspecified : Decoder {}
decodeUnspecified =
    succeed {}


decodeListOfStrings : Decoder { default : List String, example : Maybe (List String) }
decodeListOfStrings =
    map2 (\default example -> { default = default, example = example })
        (field "default" (list string))
        (maybe (field "example" (list string)))


decodeDict : Decoder { default : Dict.Dict String String, example : Maybe (Dict.Dict String String) }
decodeDict =
    map2 (\default example -> { default = default, example = example })
        (field "default" (dict string))
        (maybe (field "example" (dict string)))


decodeListOfDict : Decoder { default : List (Dict.Dict String String), example : Maybe (List (Dict.Dict String String)) }
decodeListOfDict =
    map2 (\default example -> { default = default, example = example })
        (field "default" (list (dict string)))
        (maybe (field "example" (list (dict string))))


decodeInt : Decoder { default : Int, example : Maybe Int }
decodeInt =
    map2 (\default example -> { default = default, example = example })
        (field "default" int)
        (maybe (field "example" int))


decodeNullOrInt : Decoder { default : Maybe Int, example : Maybe Int }
decodeNullOrInt =
    map2 (\default example -> { default = default, example = example })
        (field "default" (nullable int))
        (maybe (field "example" int))


decodeOptions : Decoder (Dict.Dict String ConfigOption)
decodeOptions =
    dict decodeOption


getNixosOptions =
    Http.send InitNixosOptions
        (Http.get "/options" decodeOptions)


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
