module Style exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


layoutStyle : Html.Attribute msg
layoutStyle =
    style
        [ ( "background-image", "url(\"logo.svg\")" )
        , ( "background-attachment", "fixed" )
        , ( "background-position", "center" )
        , ( "background-repeat", "no-repeat" )
        , ( "background-size", "contain" )
        , ( "background-color", "dimgrey" )
        , ( "min-height", "100vh" )
        , ( "display", "flex" )
        , ( "flex-flow", "row wrap" )
        ]


formStyle : Html.Attribute msg
formStyle =
    style
        [ ( "width", "calc(80vw - 2em)" )
        , ( "padding", "1em" )
        , ( "background", "rgba(255,255,255,0.6)" )
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
    style
        [ ( "display", "inline-block" )
        , ( "width", "20vw" )
        , ( "height", "100vh" )
        , ( "background", "rgba(81,170,255,0.8)" )
        ]


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
