module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Http
import List
import Maybe exposing (withDefault)
import Style exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    case model.step of
        LanguageStep ->
            languageStepView model

        PartitionStep ->
            partitionStepView model

        default ->
            usersStepView model


usersStepView : Model -> Html Msg
usersStepView model =
    mainLayout model
        [ h1 [] [ text "NixOS Installer" ]
        , div [ formGroupStyle ]
            [ label [ for "timezone" ] [ text "Timezone:" ]
            , select [ onInput SetTimezone, id "timezone", name "timezone" ]
                (List.append
                    [ option [] [] ]
                    (List.map timezoneItem model.timezones)
                )
            ]
        , div [ formGroupStyle, for "hostname" ]
            [ label [] [ text "Host name:" ]
            , input [ onInput SetHostname, id "hostname", name "hostname" ] []
            ]
        , div [ formGroupStyle, for "username" ]
            [ label [] [ text "Username:" ]
            , input [ onInput SetUsername, id "username", name "username" ] []
            ]
        , div [ formGroupStyle, for "password" ]
            [ label [] [ text "Password:" ]
            , input [ onInput SetPassword, id "password", type_ "password", name "password" ] []
            ]
        , button
            [ successButtonStyle
            , onClick SaveConfig
            ]
            [ text "Save configuration" ]
        , input
            [ type_ "submit"
            , dangerButtonStyle
            , Html.Attributes.value "Perform Installation"
            ]
            []
        ]


languageStepView : Model -> Html Msg
languageStepView model =
    mainLayout model
        [ h1 [] [ text "Hello World!" ] ]


partitionStepView : Model -> Html Msg
partitionStepView model =
    mainLayout model
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
                        , dd [] [ text (disk.path) ]
                        , dt [] [ text "Model:" ]
                        , dd [] [ text (disk.model) ]
                        , dt [] [ text "Serial:" ]
                        , dd [] [ text (disk.serial) ]
                        , dt [] [ text "Size:" ]
                        , dd [] [ text (disk.size) ]
                        ]
            ]
        ]


navLink model step label =
    a [ navLinkStyle (model.step == step), onClick (OpenStep step) ] [ text label ]


mainLayout model children =
    div [ backgroundStyle ]
        [ div [ navStyle ]
            [ navLink model LanguageStep "Language"
            , navLink model LocationStep "Location"
            , navLink model KeyboardStep "Keyboard"
            , navLink model PartitionStep "Partition"
            , navLink model UsersStep "Users"
            , navLink model OverlayStep "Overlay"
            ]
        , div [ formStyle ] children
        ]


init : ( Model, Cmd Msg )
init =
    ( Model
        LanguageStep
        Nothing
        Nothing
        ""
        ""
        ""
        []
        []
    , Cmd.batch [ getDisks, getTimezones ]
    )


type alias Disk =
    { path : String, model : String, serial : String, size : String }


nullDisk =
    { path = "", model = "", serial = "", size = "" }


type alias Timezone =
    { country : String, coords : String, name : String }


type Step
    = LanguageStep
    | LocationStep
    | KeyboardStep
    | PartitionStep
    | UsersStep
    | OverlayStep


type alias Model =
    { step : Step
    , disk : Maybe Disk
    , timezone : Maybe Timezone
    , username : String
    , hostname : String
    , password : String
    , disks : List Disk
    , timezones : List Timezone
    }


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
    | OpenStep Step


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDisks (Ok newDisks) ->
            ( { model | disks = newDisks }, Cmd.none )

        NewDisks (Err _) ->
            ( model, Cmd.none )

        NewTimezones (Ok newTimezones) ->
            ( { model | timezones = (List.sortBy .name newTimezones) }, Cmd.none )

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
            ( { model | timezone = findTimezone model timezoneName }, Cmd.none )

        SetUsername new ->
            ( { model | username = new }, Cmd.none )

        SetHostname new ->
            ( { model | hostname = new }, Cmd.none )

        SetPassword new ->
            ( { model | password = new }, Cmd.none )

        OpenStep newStep ->
            ( { model | step = newStep }, Cmd.none )


findDisk : Model -> String -> Maybe Disk
findDisk model diskPath =
    List.head (List.filter (\disk -> disk.path == diskPath) model.disks)


findTimezone : Model -> String -> Maybe Timezone
findTimezone model timezoneName =
    List.head (List.filter (\timezone -> timezone.name == timezoneName) model.timezones)


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
    (Http.send NewDisks (Http.get "http://localhost:8081/disks" decodeDisks))


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
    (Http.send NewTimezones (Http.get "http://localhost:8081/timezones" decodeTimezones))


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
          , Json.Encode.object
                [ ( "timeZone"
                  , case model.timezone of
                        Just timezone ->
                            Json.Encode.string timezone.name

                        Nothing ->
                            Json.Encode.null
                  )
                ]
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
          , Json.Encode.object
                [ ( "hostName"
                  , Json.Encode.string
                        model.hostname
                  )
                ]
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


timezoneItem : { a | name : String } -> Html msg
timezoneItem timezone =
    option [ Html.Attributes.value timezone.name ] [ text timezone.name ]
