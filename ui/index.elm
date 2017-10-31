module Main exposing (..)

import Html exposing (text, div, h1, input, label, select, option, img, form)
import Html.Attributes exposing (class, name, type_, for, id, value, style, src)


backgroundStyle =
    style
        [ ( "background-image", "url(\"nixos.svg\")" )
        , ( "background-attachment", "fixed" )
        , ( "background-position", "center" )
        , ( "background-repeat", "no-repeat" )
        , ( "background-size", "contain" )
        , ( "width", "100vw" )
        , ( "min-height", "100vh" )
        ]


formStyle =
    style
        [ ( "margin", "5vw" )
        , ( "padding", "5vw" )
        , ( "background"
          , "rgba(255,255,255,0.6)"
          )
        ]


formGroupStyle =
    style [ ( "margin", "2em" ) ]


successButtonStyle =
    style
        [ ( "backgroundColor", "#66aa66" )
        , ( "height", "3em" )
        , ( "width", "25%" )
        , ( "margin", "3em" )
        ]


dangerButtonStyle =
    style
        [ ( "backgroundColor", "#aa6666" )
        , ( "height", "3em" )
        , ( "width", "25%" )
        , ( "margin", "3em" )
        ]


main =
    div [ backgroundStyle ]
        [ form [ formStyle ]
            [ h1 [] [ text "NixOS Installer" ]
            , div [ formGroupStyle ]
                [ label [ for "disk" ] [ text "Choose Installation Disk:" ]
                , select [ id "disk", name "disk" ]
                    [ option [] [ text "sda" ]
                    , option [] [ text "sdb" ]
                    ]
                ]
            , div [ formGroupStyle ]
                [ label [ for "timezone" ] [ text "Timezone:" ]
                , select [ id "timezone", name "timezone" ]
                    [ option [] [ text "hello" ]
                    , option [] [ text "foo" ]
                    ]
                ]
            , div [ formGroupStyle, for "username" ]
                [ label [] [ text "Username:" ]
                , input [ id "username", type_ "text", name "username" ] []
                ]
            , div [ formGroupStyle, for "password" ]
                [ label [] [ text "Password:" ]
                , input [ id "password", type_ "password", name "password" ] []
                ]
            , input [ type_ "submit", successButtonStyle, value "Save configuration.nix" ] []
            , input [ type_ "submit", dangerButtonStyle, value "Perform Installation" ] []
            ]
        ]
