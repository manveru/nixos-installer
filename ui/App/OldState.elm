module Main exposing (..)

import App.Types exposing (..)
import Array exposing (Array)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import InstallerTranslation
    exposing
        ( defaultLanguage
        , defaultTranslation
        , languages
        , t
        )
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import List
import Material
import Material.Grid exposing (Device(..), cell, grid, size)
import Material.Layout as Layout
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Typography as Typo
import Maybe
import Mustache
import Navigation
import NixosOptions exposing (getInitialNixosOptions, nixosOptionsView)
import T
import TimezoneTab.State
import TimezoneTab.Types
import TimezoneTab.View
import Translator exposing (Translator)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = .mdl >> Layout.subs Mdl
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        fallBackTranslator =
            Translator.makeDefaultTranslator T.defaultTranslation

        translator =
            Translator.updateTranslations defaultLanguage.translation fallBackTranslator
    in
    ( { history = [ location ]
      , language = defaultLanguage
      , translator = translator
      , disk = Nothing
      , username = ""
      , fullname = ""
      , hostname = ""
      , userPass = ""
      , userPassAgain = ""
      , rootPass = ""
      , rootPassAgain = ""
      , disks = []
      , nixosOptions = []
      , mdl = Layout.setTabsWidth 600 Material.model
      , focus = False
      , selectedTab = 6
      , timezoneTab = TimezoneTab.State.init
      }
    , Cmd.batch
        [ getDisks
        , Layout.sub0 Mdl
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            ( { model | history = location :: model.history }, Cmd.none )

        NixosOptions (Ok options) ->
            ( { model | nixosOptions = options }, Cmd.none )

        NixosOptions (Err _) ->
            ( { model | nixosOptions = [ "fail" ] }, Cmd.none )

        NewDisks (Ok newDisks) ->
            ( { model | disks = newDisks }, Cmd.none )

        NewDisks (Err _) ->
            ( model, Cmd.none )

        SetDisk diskPath ->
            ( { model | disk = findDisk model diskPath }, Cmd.none )

        SavedConfig (Err _) ->
            ( model, Cmd.none )

        SavedConfig (Ok _) ->
            ( model, Cmd.none )

        SaveConfig ->
            ( model, saveConfig model )

        SetUsername new ->
            ( { model | username = new }, Cmd.none )

        SetFullname new ->
            ( { model | fullname = new }, Cmd.none )

        SetHostname new ->
            ( { model | hostname = new }, Cmd.none )

        SetUserPass new ->
            ( { model | userPass = new }, Cmd.none )

        SetUserPassAgain new ->
            ( { model | userPassAgain = new }, Cmd.none )

        SetRootPassword new ->
            ( { model | rootPass = new }, Cmd.none )

        SetRootPasswordAgain new ->
            ( { model | rootPassAgain = new }, Cmd.none )

        SetLanguage languageName ->
            let
                found =
                    findLanguage languageName
            in
            ( { model
                | language = found
                , translator = Translator.updateTranslations found.translation model.translator

                -- , timezone = findTimezone model.timezones found.timezone
              }
            , Cmd.none
            )

        SetKeyboard keyboard ->
            ( { model | keyboard = findKeyboard keyboard }, Cmd.none )

        SelectTab n ->
            ( { model | selectedTab = n }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


tabs : Array ( T.T, Model -> List (Material.Grid.Cell Msg) )
tabs =
    Array.fromList
        [ T.Language => languageView
        , T.Timezone => timezoneTab
        , T.Keyboard => keyboardView
        , T.Partition => partitionView
        , T.Users => usersView
        , T.Overlay => overlayView
        , T.NixosOptions => nixosOptionsView
        ]


timezoneTab model =
    [ cell [ fullWidth ] [ TimezoneTab.View.view model.timezoneTab ]
    , cell [ fullWidth ] []
    ]


view : Model -> Html Msg
view model =
    let
        tab =
            Maybe.withDefault ( T.Language, languageView )
                (Array.get model.selectedTab tabs)
    in
    mainLayout model (Tuple.second tab model)


mainLayout : Model -> List (Material.Grid.Cell Msg) -> Html Msg
mainLayout model children =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.selectedTab model.selectedTab
        , Layout.onSelectTab SelectTab
        ]
        { header =
            [ Layout.row []
                [ Layout.title [] [ img [ height 24, src "https://raw.githubusercontent.com/NixOS/nixos-homepage/master/logo/nixos-lores.png" ] [], text " Installer" ]
                , Layout.spacer
                , Layout.navigation []
                    [ Layout.link
                        [ Layout.href "/nixos/index.html" ]
                        [ text "NixOS Manual" ]
                    ]
                ]
            ]
        , drawer = []
        , tabs =
            ( Array.toList
                (Array.map (\tab -> t model (Tuple.first tab)) tabs)
            , []
            )
        , main =
            [ grid []
                [ cell [ Material.Grid.size All 7 ] [ grid [] children ]
                , cell [ Material.Grid.size All 5 ]
                    [ pre [] [ text (configNix model) ] ]
                ]
            ]
        }


overlayView : Model -> List (Material.Grid.Cell Msg)
overlayView model =
    [ cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ text "Choose Preset Overlays" ] ]
    , cell [ fullWidth ]
        [ select []
            [ option [] [ text "Desktop" ]
            , option [] [ text "Server" ]
            , option [] [ text "Gaming" ]
            ]
        ]
    ]


fullWidth : Options.Style a
fullWidth =
    Material.Grid.size All 12


obfuscatePass : String -> String -> String
obfuscatePass pass again =
    if pass == again then
        String.pad (String.length pass * 2) '*' "*"
    else
        ""


configNix : Model -> String
configNix model =
    let
        keyboard =
            Tuple.second model.keyboard

        disk =
            Maybe.withDefault nullDisk model.disk
    in
    Mustache.render
        [ Mustache.Variable "i18n.defaultLocale" model.language.locale
        , Mustache.Variable "i18n.consoleKeyMap" keyboard.layout
        , Mustache.Variable "kbLayout" keyboard.layout
        , Mustache.Variable "kbVariant" keyboard.variant
        , Mustache.Variable "kbname" keyboard.name
        , Mustache.Variable "username" model.username
        , Mustache.Variable "fullname" model.fullname
        , Mustache.Variable "userPass" (obfuscatePass model.userPass model.userPassAgain)
        , Mustache.Variable "disk.size" disk.size
        , Mustache.Variable "disk.model" disk.model
        , Mustache.Variable "disk.path" disk.path
        ]
        """
{config, pkgs, lib, ... }: {
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.grub = {
    version = 2;
    device = "/dev/{{disk.path}}";
    extraConfig = "serial; terminal_output.serial";
    fsIdentifier = "uuid";
  };

  time.timeZone = "TODO";

  i18n = {
    defaultLocale = "{{i18n.defaultLocale}}";
    consoleKeyMap = "{{i18n.consoleKeyMap}}";
  };

  users.extraUsers.{{username}} = {
    isNormalUser = true;
    description = "{{fullname}}";
    initialPassword = "{{userPass}}";
  };

  services = {
    xserver = {
      enable = true;
      # {{kbname}}
      layout = "{{kbLayout}}";
      xkbVariant = "{{kbVariant}}";
    };
  };
}
  """


saveEncoder : Model -> Json.Encode.Value
saveEncoder model =
    Json.Encode.object
        [ -- ( "disk"
          -- , case model.disk of
          --       Just disk ->
          --           Json.Encode.string disk.path
          --       Nothing ->
          --           Json.Encode.null
          -- )
          ( "users"
          , Json.Encode.object
                [ ( "extraUsers"
                  , Json.Encode.object
                        [ ( model.username
                          , Json.Encode.object
                                [ ( "name", Json.Encode.string model.username )
                                , ( "initialPassword", Json.Encode.string model.userPass )
                                ]
                          )
                        ]
                  )
                ]
          )
        , ( "networking"
          , Json.Encode.object [ ( "hostName", Json.Encode.string model.hostname ) ]
          )
        ]


saveConfigDecoder : Decoder String
saveConfigDecoder =
    Json.Decode.field "id_token" Json.Decode.string


postSaveConfig : Model -> Http.Request String
postSaveConfig model =
    let
        body =
            model |> saveEncoder |> Http.jsonBody
    in
    Http.post "http://localhost:8081/save" body saveConfigDecoder


saveConfig : Model -> Cmd Msg
saveConfig model =
    Http.send SavedConfig (postSaveConfig model)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
