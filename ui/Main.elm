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
import Material.Layout as Layout
import Material.Options as Options
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
      , hostname = ""
      , password = ""
      , disks = []
      , timezones = []
      , mdl = Material.model
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
    , hostname : String
    , password : String
    , disks : List Disk
    , timezones : List Timezone
    , mdl : Material.Model
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
    | SetPassword String
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

        SetHostname new ->
            ( { model | hostname = new }, Cmd.none )

        SetPassword new ->
            ( { model | password = new }, Cmd.none )

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

        Mdl _ ->
            ( model, Cmd.none )


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
                languageStepView model

            1 ->
                partitionStepView model

            2 ->
                locationStepView model

            3 ->
                usersStepView model

            default ->
                overlayStepView model
        )


usersStepView : Model -> List (Html Msg)
usersStepView model =
    [ div [] [] ]


locationStepView : Model -> List (Html Msg)
locationStepView model =
    [ div []
        [ label [ for "timezone" ] [ text "Timezone:" ]
        , select [ onInput SetTimezone, id "timezone", name "timezone" ]
            (List.map (timezoneItem model.timezone) model.timezones)
        ]
    ]


overlayStepView : Model -> List (Html Msg)
overlayStepView model =
    [ div [ for "hostname" ]
        [ label [] [ text "Host name:" ]
        , input [ onInput SetHostname, id "hostname", name "hostname" ] []
        ]
    , div [ for "username" ]
        [ label [] [ text "Username:" ]
        , input [ onInput SetUsername, id "username", name "username" ] []
        ]
    , div [ for "password" ]
        [ label [] [ text "Password:" ]
        , input [ onInput SetPassword, id "password", type_ "password", name "password" ] []
        ]
    , button
        [ onClick SaveConfig
        ]
        [ text "Save configuration" ]
    , input
        [ type_ "submit"
        , Html.Attributes.value "Perform Installation"
        ]
        []
    ]


languageStepView : Model -> List (Html Msg)
languageStepView model =
    [ Options.styled p [ Typo.center, Typo.display2 ] [ text "Welcome to the NixOS Installer." ]
    , Options.styled p [ Typo.subhead ] [ text "Please select your language and continue to the next step." ]
    , select [ onInput SetLanguage, id "language", name "language" ]
        (List.map (languageItem model.language) languages)
    ]


partitionStepView : Model -> List (Html Msg)
partitionStepView model =
    [ div
        []
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


mainLayout : Model -> List (Html Msg) -> Html Msg
mainLayout model children =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.waterfall True
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
        , main = children
        }


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
                                , ( "initialPassword", Json.Encode.string model.password )
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
