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
        , ( "display", "flex" )
        ]


formStyle : Html.Attribute msg
formStyle =
    style
        [ ( "margin", "0 0" )
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


navStyle : Html.Attribute msg
navStyle =
    style [ ( "display", "inline-block" ), ( "width", "20vw" ) ]


navLinkStyle : Bool -> Attribute msg
navLinkStyle active =
    style
        [ ( "display", "block" )
        , ( "line-height", "1em" )
        , ( "border", "1px solid black" )
        , ( "padding", "1em" )
        , ( "font-size", "1.5em" )
        , ( "background"
          , if active then
                "#5EFFF8"
            else
                "#51AAFF"
          )
        ]
