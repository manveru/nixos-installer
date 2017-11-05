module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import I18Next exposing (Translations, t)
import InstallerTranslation
    exposing
        ( Language
        , defaultLanguage
        , defaultTranslation
        , languages
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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { language = defaultLanguage
      , translation = defaultTranslation
      , step = LanguageStep
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
      , selectedTab = 0
      }
    , Cmd.batch [ getDisks, getTimezones ]
    )


type alias Model =
    { language : Language
    , translation : Translations
    , step : Step
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
    | SelectTab Int
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                , translation = found.translation
                , timezone = findTimezone model.timezones found.timezone
              }
            , Cmd.none
            )

        SelectTab n ->
            ( { model | selectedTab = n }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


type Step
    = LanguageStep
    | LocationStep
    | KeyboardStep
    | PartitionStep
    | UsersStep
    | OverlayStep


view : Model -> Html Msg
view model =
    mainLayout model
        (case model.selectedTab of
            0 ->
                languageView model

            1 ->
                locationView model

            3 ->
                partitionView model

            4 ->
                usersView model

            default ->
                overlayView model
        )


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
                [ Layout.title [] [ text "NixOS Installer" ]
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
            ( [ text (t model.translation "language")
              , text (t model.translation "location")
              , text (t model.translation "keyboard")
              , text (t model.translation "partition")
              , text (t model.translation "users")
              , text (t model.translation "overlay")
              ]
            , []
            )
        , main =
            [ grid []
                [ cell [ Material.Grid.size All 8 ] [ grid [] children ]
                , cell [ Material.Grid.size All 4 ]
                    [ pre []
                        [ text
                            (configNix
                                model
                            )
                        ]
                    ]
                ]
            ]
        }


languageView : Model -> List (Material.Grid.Cell Msg)
languageView model =
    [ cell [ fullWidth ] [ Options.styled p [ Typo.center, Typo.display2 ] [ text "Welcome to the\n        NixOS Installer." ] ]
    , cell [ fullWidth ] [ Options.styled p [ Typo.display1 ] [ text "Configure Language" ] ]
    , cell [ fullWidth ]
        [ Options.styled p [ Typo.subhead ] [ text "Please select your language and continue to the next step." ]
        , select [ onInput SetLanguage, id "language", name "language" ]
            (List.map (languageItem model.language) languages)
        ]
    ]


overlayView : Model -> List (Material.Grid.Cell Msg)
overlayView model =
    [ cell [] [ h4 [] [ text "Keyboard" ] ] ]


locationView : Model -> List (Material.Grid.Cell Msg)
locationView model =
    [ cell []
        [ label [ for "timezone" ] [ text "Timezone:" ]
        , select [ onInput SetTimezone, id "timezone", name "timezone" ]
            (List.map (timezoneItem model.timezone) model.timezones)
        ]
    ]


usersView : Model -> List (Material.Grid.Cell Msg)
usersView model =
    [ cell [ fullWidth ] [ Options.styled p [ Typo.display1 ] [ text "Configure Users" ] ]
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


passInput n model key value event valueAgain eventAgain label caption =
    cell [ fullWidth ]
        [ grid []
            [ cell [ fullWidth ]
                [ Options.styled p [ Typo.body2 ] [ text label ] ]
            , cell [ Material.Grid.size All 2 ]
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
            , cell [ Material.Grid.size All 2 ]
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
            [ cell []
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


configNix model =
    String.join "" [ """
{config, pkgs, lib, ... }:
with builtins;
{
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.grub = {
    version = 2;
    device = "/dev/vda";
    extraConfig = "serial; terminal_output.serial";
    fsIdentifier = "uuid";
  };

  time.timeZone = \"""", model.timezone.name, """";

  i18n = {
    defaultLocale = \"""", model.language.locale, """";
  };

  users.extraUsers.""", model.username, """ = {
    isNormalUser = true;
    home = "/home/""", model.username, """";
    description = \"""", model.fullname, """";
  };
}
  """ ]


partitionView : Model -> List (Material.Grid.Cell Msg)
partitionView model =
    [ cell []
        [ label [ for "disk" ] [ text "Install to:" ]
        , select [ onInput SetDisk, id "disk", name "disk" ]
            (List.append
                [ option [] [] ]
                (List.map diskItem model.disks)
            )
        , case model.disk of
            Nothing ->
                div [] [ text "Select a disk" ]

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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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
