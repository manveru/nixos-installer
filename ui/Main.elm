module Main exposing (main)

import App.State
import App.Types exposing (..)
import Html exposing (Html, div, img, pre, text)
import Html.Attributes exposing (height, src)
import KeyboardTab.View
import LanguageTab.View
import Material.Grid exposing (Device(..), cell, grid, size)
import Material.Layout as Layout
import Navigation
import NixosConfiguration
import PartitionTab.View
import SHA512Crypt
import String
import T exposing (t)
import TimezoneTab.View
import UserTab.View


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , subscriptions = subscriptions
        , update = App.State.update
        , view = view
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        history =
            [ location ]

        ( model, cmd ) =
            App.State.init history
    in
    ( model, Cmd.batch [ Layout.sub0 Mdl, cmd ] )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Layout.subs Mdl model.mdl
        ]


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.selectedTab model.selectedTab
        , Layout.onSelectTab SelectTab
        ]
        { header = header model
        , drawer = []
        , tabs = tabs model
        , main = [ render model ]
        }


header : Model -> List (Html msg)
header model =
    [ Layout.row []
        [ Layout.title []
            [ img [ height 24, src "assets/logo.svg" ] []
            , t model T.InstallerTitle
            ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link
                [ Layout.href "/nixos/index.html" ]
                [ t model T.NixosManual ]
            ]
        ]
    ]


tabs : Model -> ( List (Html msg), List a )
tabs model =
    ( [ t model T.Language
      , t model T.Timezone
      , t model T.Keyboard
      , t model T.Partition
      , t model T.Users
      ]
    , []
    )


render : Model -> Html Msg
render model =
    body model
        (case model.selectedTab of
            0 ->
                LanguageTab.View.view model

            1 ->
                TimezoneTab.View.view model

            2 ->
                KeyboardTab.View.view model

            3 ->
                PartitionTab.View.view model

            4 ->
                UserTab.View.view model

            _ ->
                LanguageTab.View.view model
        )


body : Model -> Html Msg -> Html Msg
body model content =
    grid []
        [ cell [ Material.Grid.size All 8 ] [ content ]
        , cell [ Material.Grid.size All 4 ] [ pre [] [ text (NixosConfiguration.config model) ] ]
        , cell [ Material.Grid.size All 4 ] [ div [] [ text (String.slice 0 1000 model.error) ] ]
        ]



--update : Msg -> Model -> ( Model, Cmd Msg )
--update message model =
--    case message of
--        PartitionMsg msg ->
--            let
--                ( mdl, cmd ) =
--                    PartitionTab.State.update msg model.partition
--            in
--            ( { model | partition = mdl }, cmd |> Cmd.map PartitionMsg )
--
--        LanguageMsg msg ->
--            let
--                ( mdl, cmd ) =
--                    LanguageTab.State.update msg model.language
--            in
--            ( { model
--                | language = mdl
--                , translator =
--                    Translator.updateTranslations
--                        mdl.language.translation
--                        model.translator
--              }
--            , cmd |> Cmd.map LanguageMsg
--            )
--
--        TimezoneMsg msg ->
--            let
--                ( mdl, cmd ) =
--                    TimezoneTab.State.update msg model.timezone
--            in
--            ( { model | timezone = mdl }, cmd |> Cmd.map TimezoneMsg )
--
--        KeyboardMsg msg ->
--            let
--                ( mdl, cmd ) =
--                    KeyboardTab.State.update msg model
--            in
--            ( mdl, cmd |> Cmd.map KeyboardMsg )
--
--        UserMsg msg ->
--            let
--                ( mdl, cmd ) =
--                    UserTab.State.update msg model
--            in
--            ( mdl, cmd |> Cmd.map UserMsg )
