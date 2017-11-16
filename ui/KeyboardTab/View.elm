module KeyboardTab.View exposing (..)

import App.Types exposing (..)
import Dict
import Html exposing (Html, option, p, select, text)
import Html.Attributes exposing (id, name, selected)
import Html.Events exposing (onInput)
import KeyboardTab.Layouts exposing (..)
import Material.Grid exposing (cell, grid)
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Typography as Typo
import Style exposing (..)


view : Model -> Html Msg
view model =
    grid []
        (List.map (\h -> h model)
            [ title, subtitle, pickKeyboard, tryKeyboard ]
        )


title : s -> Material.Grid.Cell Msg
title model =
    cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ text "Configure Keyboard" ] ]


subtitle : s -> Material.Grid.Cell Msg
subtitle model =
    cell [ fullWidth ]
        [ Options.styled p [ Typo.subhead ] [ text "Pick a keyboard layout." ]
        ]


pickKeyboard : Model -> Material.Grid.Cell Msg
pickKeyboard model =
    cell [ fullWidth ]
        [ select [ onInput ChooseKeyboard, id "keyboard", name "keyboard" ]
            (Dict.values
                (Dict.map (item model.keyboard.keyboard) all)
            )
        ]


tryKeyboard : Model -> Material.Grid.Cell Msg
tryKeyboard model =
    cell [ fullWidth ]
        [ Textfield.render Mdl
            [ 99 ]
            model.mdl
            [ Textfield.label "Type here to try out your keyboard configuration."
            , Textfield.floatingLabel
            , Textfield.textarea
            , Options.id "try-keyboard"
            ]
            []
        ]


item :
    ( Key, KeyboardLayout )
    -> String
    -> KeyboardLayout
    -> Html msg
item current key given =
    option
        [ Html.Attributes.value key
        , selected (Tuple.second current == given)
        ]
        [ text given.name ]
