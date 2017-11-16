module LanguageTab.View exposing (..)

import App.Types exposing (..)
import Html exposing (Html, option, p, select, text)
import Html.Attributes exposing (id, name, selected, value)
import Html.Events exposing (onInput)
import Material.Grid exposing (cell, grid)
import Material.Options as Options
import Material.Typography as Typo
import Style exposing (..)
import T exposing (t)


view : Model -> Html Msg
view model =
    grid []
        [ cell [ fullWidth ]
            [ Options.styled p [ Typo.display1 ] [ t model T.Language ] ]
        , cell [ fullWidth ]
            [ Options.styled p [ Typo.subhead ] [ t model T.LanguageCaption ]
            , select [ onInput ChooseLanguage, id "language", name "language" ]
                (List.map (languageItem model.language) T.languages)
            ]
        ]


languageItem : Language -> Language -> Html msg
languageItem current language =
    option
        [ value language.name
        , selected (current == language)
        ]
        [ text language.name ]
