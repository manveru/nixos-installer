module Style exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


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
