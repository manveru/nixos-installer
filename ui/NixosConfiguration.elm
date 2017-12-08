module NixosConfiguration exposing (config)

import App.Types exposing (..)
import Dict
import Regex exposing (..)


getKey : String -> String
getKey match =
    match
        |> String.dropLeft 2
        |> String.dropRight 2


getValue : String -> Dict.Dict String ConfigOption -> String
getValue key variables =
    let
        found =
            Dict.get key variables
    in
    case found of
        Nothing ->
            ""

        Just option ->
            case option of
                IntConfig conf ->
                    toString conf.default

                _ ->
                    "????"


replaceVariables : Dict.Dict String ConfigOption -> String -> String
replaceVariables variables template =
    replace All
        (regex "{{[^}]+}}")
        (\{ match } ->
            getValue (getKey match) variables
        )
        template


vars : Model -> Dict.Dict String ConfigOption
vars model =
    model.config


config : Model -> String
config model =
    replaceVariables (vars model)
        """
{config, pkgs, ...}: {
  networking.hostname = "{{networking.hostname}}";
}
  """
