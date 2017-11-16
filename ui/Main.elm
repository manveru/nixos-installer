module Main exposing (main)

import App.State
import App.Types exposing (..)
import Html exposing (Html, img, pre, text)
import Html.Attributes exposing (height, src)
import KeyboardTab.View
import LanguageTab.View
import Material
import Material.Layout as Layout
import Navigation
import PartitionTab.View
import T exposing (t)
import TimezoneTab.View
import Translator
import UserTab.View


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { history = [ location ] }

        appInit =
            App.State.init
    in
    ( { appInit | history = [ location ] }
    , Cmd.batch
        [ keyboardCmds |> Cmd.map KeyboardMsg
        , userCmds |> Cmd.map UserMsg
        , timezoneCmds |> Cmd.map TimezoneMsg
        , languageCmds |> Cmd.map LanguageMsg
        , partitionCmds |> Cmd.map PartitionMsg
        , Layout.sub0 Mdl
        ]
    )


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
    case model.selectedTab of
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


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PartitionMsg msg ->
            let
                ( mdl, cmd ) =
                    PartitionTab.State.update msg model.partition
            in
            ( { model | partition = mdl }, cmd |> Cmd.map PartitionMsg )

        LanguageMsg msg ->
            let
                ( mdl, cmd ) =
                    LanguageTab.State.update msg model.language
            in
            ( { model
                | language = mdl
                , translator =
                    Translator.updateTranslations
                        mdl.language.translation
                        model.translator
              }
            , cmd |> Cmd.map LanguageMsg
            )

        TimezoneMsg msg ->
            let
                ( mdl, cmd ) =
                    TimezoneTab.State.update msg model.timezone
            in
            ( { model | timezone = mdl }, cmd |> Cmd.map TimezoneMsg )

        KeyboardMsg msg ->
            let
                ( mdl, cmd ) =
                    KeyboardTab.State.update msg model
            in
            ( mdl, cmd |> Cmd.map KeyboardMsg )

        UserMsg msg ->
            let
                ( mdl, cmd ) =
                    UserTab.State.update msg model
            in
            ( mdl, cmd |> Cmd.map UserMsg )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ UserTab.State.subscriptions model.user |> Sub.map UserMsg
        , Layout.subs Mdl model.mdl
        ]
