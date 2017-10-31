module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (..)
import Http
import List


type alias Disk =
    { path : String, model : String, serial : String, size : String }


type alias Timezone =
    { country : String, coords : String, name : String }


type alias Model =
    { disks : List Disk, timezones : List Timezone }


init : ( Model, Cmd Msg )
init =
    ( Model [] [], Cmd.batch [ getDisks, getTimezones ] )


type Msg
    = NewDisks (Result Http.Error (List Disk))
    | NewTimezones (Result Http.Error (List Timezone))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDisks (Ok newDisks) ->
            ( { model | disks = newDisks }, Cmd.none )

        NewDisks (Err _) ->
            ( model, Cmd.none )

        NewTimezones (Ok newTimezones) ->
            ( { model | timezones = newTimezones }, Cmd.none )

        NewTimezones (Err _) ->
            ( model, Cmd.none )


decodeDisks : Decoder (List Disk)
decodeDisks =
    Json.Decode.list
        (map4
            Disk
            (at [ "path" ] string)
            (at [ "model" ] string)
            (at [ "serial" ] string)
            (at [ "size" ] string)
        )


getDisks : Cmd Msg
getDisks =
    (Http.send NewDisks (Http.get "http://localhost:8081/disks" decodeDisks))


decodeTimezones : Decoder (List Timezone)
decodeTimezones =
    Json.Decode.list
        (map3
            Timezone
            (at [ "country" ] string)
            (at [ "coords" ] string)
            (at [ "name" ] string)
        )


getTimezones : Cmd Msg
getTimezones =
    (Http.send NewTimezones (Http.get "http://localhost:8081/timezones" decodeTimezones))


backgroundStyle : Html.Attribute msg
backgroundStyle =
    style
        [ ( "background-image", "url(\"logo.svg\")" )
        , ( "background-attachment", "fixed" )
        , ( "background-position", "center" )
        , ( "background-repeat", "no-repeat" )
        , ( "background-size", "contain" )
        , ( "background-color", "dimgrey" )
        , ( "width", "100vw" )
        , ( "min-height", "100vh" )
        ]


formStyle : Html.Attribute msg
formStyle =
    style
        [ ( "margin", "0 5vw" )
        , ( "padding", "5vw" )
        , ( "background"
          , "rgba(255,255,255,0.6)"
          )
        ]


formGroupStyle : Html.Attribute msg
formGroupStyle =
    style [ ( "margin", "2em" ) ]


successButtonStyle : Html.Attribute msg
successButtonStyle =
    style
        [ ( "background-color", "#66aa66" )
        , ( "height", "3em" )
        , ( "width", "25%" )
        , ( "margin", "3em" )
        ]


dangerButtonStyle : Html.Attribute msg
dangerButtonStyle =
    style
        [ ( "background-color", "#aa6666" )
        , ( "height", "3em" )
        , ( "width", "25%" )
        , ( "margin", "3em" )
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


diskItem : Disk -> Html msg
diskItem disk =
    option [ Html.Attributes.value disk.path ]
        [ text disk.model
        , text (" (" ++ disk.serial)
        , text (" with " ++ disk.size ++ ")")
        ]


timezoneItem : { a | name : String } -> Html msg
timezoneItem timezone =
    option [ Html.Attributes.value timezone.name ] [ text timezone.name ]


view : Model -> Html Msg
view model =
    div [ backgroundStyle ]
        [ Html.form [ formStyle ]
            [ h1 [] [ text "NixOS Installer" ]
            , div [ formGroupStyle ]
                [ label [ for "disk" ] [ text "Install to:" ]
                , select [ id "disk", name "disk" ]
                    (List.map diskItem model.disks)
                ]
            , div [ formGroupStyle ]
                [ label [ for "timezone" ] [ text "Timezone:" ]
                , select [ id "timezone", name "timezone" ]
                    (List.map timezoneItem model.timezones)
                ]
            , div [ formGroupStyle, for "username" ]
                [ label [] [ text "Username:" ]
                , input [ id "username", type_ "text", name "username" ] []
                ]
            , div [ formGroupStyle, for "password" ]
                [ label [] [ text "Password:" ]
                , input [ id "password", type_ "password", name "password" ] []
                ]
            , input
                [ type_ "submit"
                , successButtonStyle
                , Html.Attributes.value "Save configuration.nix"
                ]
                []
            , input
                [ type_ "submit"
                , dangerButtonStyle
                , Html.Attributes.value "Perform Installation"
                ]
                []
            ]
        ]
