module PartitionTab.View exposing (..)

import App.Types exposing (..)
import Html exposing (Html, dd, div, dl, dt, option, p, select, text)
import Html.Attributes exposing (id, name, selected, value)
import Html.Events exposing (onInput)
import Material.Grid exposing (Device(..), cell, grid, size)
import Material.Options as Options
import Material.Typography as Typo
import Style exposing (..)


view : Model -> Html Msg
view model =
    grid []
        (List.map (\h -> h model)
            [ title, subtitle, pickDisk, showDisk ]
        )


title : s -> Material.Grid.Cell msg
title _ =
    cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ text "Install Location" ] ]


subtitle : s -> Material.Grid.Cell msg
subtitle _ =
    cell [ fullWidth ]
        [ Options.styled p [ Typo.subhead ] [ text "Select a disk to format and install NixOS on." ]
        ]


pickDisk : Model -> Material.Grid.Cell Msg
pickDisk model =
    cell [ fullWidth ]
        [ select [ onInput ChooseKeyboard, id "disk", name "disk" ]
            (List.append
                [ option [] [] ]
                (List.map diskItem model.disks)
            )
        ]


showDisk : Model -> Material.Grid.Cell Msg
showDisk model =
    cell [ fullWidth ]
        [ if model.disk.path == "" then
            div [] [ text "Nothing selected" ]
          else
            dl []
                [ dt [] [ text "Path:" ]
                , dd [] [ text model.disk.path ]
                , dt [] [ text "Model:" ]
                , dd [] [ text model.disk.model ]
                , dt [] [ text "Serial:" ]
                , dd [] [ text model.disk.serial ]
                , dt [] [ text "Size:" ]
                , dd [] [ text model.disk.size ]
                ]
        ]


diskItem : Disk -> Html msg
diskItem disk =
    option [ value disk.path ]
        [ text disk.model
        , text (" (" ++ disk.size ++ ")")
        ]
