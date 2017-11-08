module Main exposing (..)

import Array exposing (Array)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import InstallerTranslation
    exposing
        ( Language
        , defaultLanguage
        , defaultTranslation
        , languages
        , t
        )
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import KeyboardLayouts
    exposing
        ( Key
        , KeyboardLayout
        , defaultKeyboardLayout
        , keyboardLayoutsAndVariants
        )
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
import T
import Timezones
import Translator exposing (Translator)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        fallBackTranslator =
            Translator.makeDefaultTranslator T.defaultTranslation

        translator =
            Translator.updateTranslations defaultLanguage.translation fallBackTranslator

        keyboard =
            ( "us"
            , Maybe.withDefault defaultKeyboardLayout
                (Dict.get "us" keyboardLayoutsAndVariants)
            )
    in
    ( { history = [ location ]
      , language = defaultLanguage
      , keyboard = keyboard
      , translator = translator
      , disk = Nothing
      , timezone = nullTimezone
      , username = ""
      , fullname = ""
      , hostname = ""
      , userPass = ""
      , userPassAgain = ""
      , rootPassword = ""
      , rootPasswordAgain = ""
      , disks = []
      , timezones = []
      , mdl = Material.model
      , focus = False
      , selectedTab = 3
      }
    , Cmd.batch [ getDisks, getTimezones ]
    )


type alias Model =
    { history : List Navigation.Location
    , language : Language
    , translator : Translator
    , keyboard : ( Key, KeyboardLayout )
    , disk : Maybe Disk
    , timezone : Timezone
    , username : String
    , fullname : String
    , hostname : String
    , userPass : String
    , userPassAgain : String
    , rootPassword : String
    , rootPasswordAgain : String
    , disks : List Disk
    , timezones : List Timezone
    , mdl : Material.Model
    , focus : Bool
    , selectedTab : Int
    }


type alias Disk =
    { path : String, model : String, serial : String, size : String }


nullDisk : Disk
nullDisk =
    { path = "", model = "", serial = "", size = "" }


type alias Timezone =
    { country : String, coords : String, name : String }


nullTimezone : Timezone
nullTimezone =
    { country = "", coords = "", name = "" }


type Msg
    = NewDisks (Result Http.Error (List Disk))
    | NewTimezones (Result Http.Error (List Timezone))
    | SavedConfig (Result Http.Error String)
    | SaveConfig
    | SetDisk String
    | SetTimezone String
    | SetHostname String
    | SetUsername String
    | SetFullname String
    | SetUserPass String
    | SetUserPassAgain String
    | SetRootPassword String
    | SetRootPasswordAgain String
    | SetLanguage String
    | SetKeyboard String
    | SelectTab Int
    | Mdl (Material.Msg Msg)
    | UrlChange Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            ( { model | history = location :: model.history }, Cmd.none )

        NewDisks (Ok newDisks) ->
            ( { model | disks = newDisks }, Cmd.none )

        NewDisks (Err _) ->
            ( model, Cmd.none )

        NewTimezones (Ok newTimezones) ->
            ( { model
                | timezones = List.sortBy .name newTimezones
                , timezone = findTimezone newTimezones model.language.timezone
              }
            , Cmd.none
            )

        NewTimezones (Err _) ->
            ( model, Cmd.none )

        SavedConfig (Err _) ->
            ( model, Cmd.none )

        SavedConfig (Ok _) ->
            ( model, Cmd.none )

        SaveConfig ->
            ( model, saveConfig model )

        SetDisk diskPath ->
            ( { model | disk = findDisk model diskPath }, Cmd.none )

        SetTimezone timezoneName ->
            ( { model | timezone = findTimezone model.timezones timezoneName }, Cmd.none )

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
            ( { model | rootPassword = new }, Cmd.none )

        SetRootPasswordAgain new ->
            ( { model | rootPasswordAgain = new }, Cmd.none )

        SetLanguage languageName ->
            let
                found =
                    findLanguage languageName
            in
            ( { model
                | language = found
                , translator = Translator.updateTranslations found.translation model.translator
                , timezone = findTimezone model.timezones found.timezone
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
        , T.Location => locationView
        , T.Keyboard => keyboardView
        , T.Partition => partitionView
        , T.Users => usersView
        , T.Overlay => overlayView
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


languageView : Model -> List (Material.Grid.Cell Msg)
languageView model =
    [ cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ text "Configure Language" ] ]
    , cell [ fullWidth ]
        [ Options.styled p [ Typo.subhead ] [ text "Please select your language and continue to the next step." ]
        , select [ onInput SetLanguage, id "language", name "language" ]
            (List.map (languageItem model.language) languages)
        ]
    ]


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


keyboardView : Model -> List (Material.Grid.Cell Msg)
keyboardView model =
    [ cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ text "Configure Keyboard" ] ]
    , cell [ fullWidth ]
        [ Options.styled p [ Typo.subhead ] [ text "Pick a keyboard layout." ]
        , select [ onInput SetKeyboard, id "language", name "language" ]
            (Dict.values
                (Dict.map (keyboardLayoutItem model.keyboard)
                    keyboardLayoutsAndVariants
                )
            )
        ]
    , cell [ fullWidth ]
        [ Textfield.render Mdl
            [ 99 ]
            model.mdl
            [ Textfield.label "Try out your keyboard configuration."
            , Textfield.floatingLabel
            , Textfield.textarea
            ]
            []
        ]
    ]


locationView : Model -> List (Material.Grid.Cell Msg)
locationView model =
    [ cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ text "Configure Location" ] ]
    , cell [ fullWidth ]
        [ Options.styled p [ Typo.subhead ] [ text """
        Select your location, so that the system can use appropriate display
        conventions for your country and set the clock to the correct local
        time.
        """ ]
        , Timezones.zones model.timezone
        , label [ for "timezone" ] [ text "Timezone:" ]
        , select [ onInput SetTimezone, id "timezone", name "timezone" ]
            (List.map (timezoneItem model.timezone) model.timezones)
        ]
    ]


usersView : Model -> List (Material.Grid.Cell Msg)
usersView model =
    [ cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ text "Configure Users" ] ]
    , formInput 10 model model.fullname "fullname" "What is your name?" SetFullname
    , formInput 20 model model.username "username" "What name do you want to use to log in?" SetUsername
    , passInput 30
        model
        "pwd"
        model.userPass
        SetUserPass
        model.userPassAgain
        SetUserPassAgain
        "Choose a password to keep your account safe."
        """
          Enter the same password twice, so that it can be checked for typing
          errors. A good password will contain a mixture of letters, numbers and
          punctuation. Should be at least eight characters long, and should be
          changed at regular intervals
        """
    , formInput 50 model model.hostname "hostname" "What is the name of this computer?" SetHostname
    , formCaption
        """
          This name will be used if you make the computer visible to others on
          the network.
        """
    , passInput 60
        model
        "admpwd"
        model.userPass
        SetRootPassword
        model.userPassAgain
        SetRootPasswordAgain
        "Choose a password for the administrator account."
        """
        Enter the same password twice, so that it can be checked for
        typing errors.
        """
    , cell [ fullWidth ]
        [ button
            [ onClick SaveConfig
            ]
            [ text "Save configuration" ]
        , input
            [ type_ "submit"
            , Html.Attributes.value "Perform Installation"
            ]
            []
        ]
    ]


fullWidth : Options.Style a
fullWidth =
    Material.Grid.size All 12


passInput :
    Int
    -> Model
    -> String
    -> String
    -> (String -> Msg)
    -> String
    -> (String -> Msg)
    -> String
    -> String
    -> Material.Grid.Cell Msg
passInput n model key value event valueAgain eventAgain label caption =
    cell [ fullWidth ]
        [ grid []
            [ cell [ fullWidth ]
                [ Options.styled p [ Typo.body2 ] [ text label ] ]
            , cell [ Material.Grid.size All 6 ]
                [ Textfield.render Mdl
                    [ n ]
                    model.mdl
                    [ Textfield.label "Enter password"
                    , Textfield.floatingLabel
                    , Textfield.password
                    , Textfield.value value
                    , Options.onInput event
                    , Options.id key
                    ]
                    []
                ]
            , cell [ Material.Grid.size All 6 ]
                [ Textfield.render Mdl
                    [ n + 1 ]
                    model.mdl
                    [ Textfield.label "Repeat password"
                    , Textfield.floatingLabel
                    , Textfield.password
                    , Textfield.value valueAgain
                    , Options.onInput eventAgain
                    , Options.id (key ++ "again")
                    ]
                    []
                ]
            , formCaption caption
            ]
        ]


formInput :
    Int
    -> Model
    -> String
    -> String
    -> String
    -> (String -> Msg)
    -> Material.Grid.Cell Msg
formInput n model value id labelName event =
    cell [ fullWidth ]
        [ grid []
            [ cell [ Material.Grid.size All 6 ]
                [ Textfield.render Mdl
                    [ n ]
                    model.mdl
                    [ Textfield.label labelName
                    , Textfield.floatingLabel
                    , Textfield.value value
                    , Options.onInput event
                    , Options.id id
                    ]
                    []
                ]
            ]
        ]


formCaption : String -> Material.Grid.Cell msg
formCaption content =
    cell [ fullWidth ]
        [ Options.styled p [ Typo.body1 ] [ text content ] ]


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
        [ Mustache.Variable "time.timezone" model.timezone.name
        , Mustache.Variable "i18n.defaultLocale" model.language.locale
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

  time.timeZone = "{{time.timezone}}";

  i18n = {
    defaultLocale = "{{i18n.defaultLocale}}";
    consoleKeyMap = "{{i18n.consoleKeyMap}}";
  };

  users.extraUsers.{{username}} = {
    isNormalUser = true;
    home = "/home/{{username}}";
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


partitionView : Model -> List (Material.Grid.Cell Msg)
partitionView model =
    [ cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ text "Install Location" ] ]
    , cell [ fullWidth ]
        [ Options.styled p [ Typo.subhead ] [ text "Select a disk to format and install NixOS on." ]
        , select [ onInput SetDisk, id "disk", name "disk" ]
            (List.append
                [ option [] [] ]
                (List.map diskItem model.disks)
            )
        ]
    , cell [ fullWidth ]
        [ case model.disk of
            Nothing ->
                div [] [ text "Nothing selected" ]

            Just disk ->
                dl []
                    [ dt [] [ text "Path:" ]
                    , dd [] [ text disk.path ]
                    , dt [] [ text "Model:" ]
                    , dd [] [ text disk.model ]
                    , dt [] [ text "Serial:" ]
                    , dd [] [ text disk.serial ]
                    , dt [] [ text "Size:" ]
                    , dd [] [ text disk.size ]
                    ]
        ]
    ]


findDisk : Model -> String -> Maybe Disk
findDisk model diskPath =
    List.head (List.filter (\disk -> disk.path == diskPath) model.disks)


findLanguage : String -> Language
findLanguage name =
    Maybe.withDefault defaultLanguage
        (List.head (List.filter (\lang -> lang.name == name) languages))


findKeyboard :
    String
    -> ( String, KeyboardLayout )
findKeyboard keyboardName =
    let
        found =
            Maybe.withDefault defaultKeyboardLayout (Dict.get keyboardName keyboardLayoutsAndVariants)
    in
    ( keyboardName, found )


findTimezone : List Timezone -> String -> Timezone
findTimezone timezones timezoneName =
    Maybe.withDefault nullTimezone
        (List.head (List.filter (\timezone -> timezone.name == timezoneName) timezones))


decodeDisks : Decoder (List Disk)
decodeDisks =
    Json.Decode.list
        (map4
            Disk
            (at [ "path" ] Json.Decode.string)
            (at [ "model" ] Json.Decode.string)
            (at [ "serial" ] Json.Decode.string)
            (at [ "size" ] Json.Decode.string)
        )


getDisks : Cmd Msg
getDisks =
    Http.send NewDisks (Http.get "http://localhost:8081/disks" decodeDisks)


diskAttribute : Maybe Disk -> (Disk -> a) -> a
diskAttribute disk extractor =
    disk |> Maybe.withDefault nullDisk |> extractor


decodeTimezones : Decoder (List Timezone)
decodeTimezones =
    Json.Decode.list
        (map3
            Timezone
            (at [ "country" ] Json.Decode.string)
            (at [ "coords" ] Json.Decode.string)
            (at [ "name" ] Json.Decode.string)
        )


getTimezones : Cmd Msg
getTimezones =
    Http.send NewTimezones (Http.get "http://localhost:8081/timezones" decodeTimezones)


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
          ( "time"
          , Json.Encode.object [ ( "timeZone", Json.Encode.string model.timezone.name ) ]
          )
        , ( "users"
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


diskItem : Disk -> Html msg
diskItem disk =
    option [ Html.Attributes.value disk.path ]
        [ text disk.model
        , text (" (" ++ disk.size ++ ")")
        ]


languageItem : Language -> Language -> Html msg
languageItem current language =
    option
        [ Html.Attributes.value language.name
        , selected (current == language)
        ]
        [ text language.name ]


timezoneItem : Timezone -> Timezone -> Html msg
timezoneItem current timezone =
    option
        [ Html.Attributes.value timezone.name
        , selected (current == timezone)
        ]
        [ text timezone.name ]


keyboardLayoutItem :
    ( Key, KeyboardLayout )
    -> String
    -> KeyboardLayout
    -> Html msg
keyboardLayoutItem current key given =
    option
        [ Html.Attributes.value key
        , selected (Tuple.second current == given)
        ]
        [ text given.name ]


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
