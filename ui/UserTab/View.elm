module UserTab.View exposing (..)

import App.Types exposing (..)
import Html exposing (Html, input, option, p, select, text)
import Material.Grid exposing (Device(..), cell, grid, size)
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Typography as Typo
import Style exposing (..)
import T exposing (t, tt)
import Translator


view :
    { a | user : Model, mdl : Store s, translator : Translator.Translator }
    -> Html Msg
view model =
    grid []
        ([ title model
         , subtitle model
         ]
            ++ List.map (\h -> h model.user model.mdl model)
                [ enterName, enterUsername, enterPassword ]
        )


title :
    { a | translator : Translator.Translator }
    -> Material.Grid.Cell msg
title tr =
    cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ t tr T.UserTab ] ]


subtitle :
    { a | translator : Translator.Translator }
    -> Material.Grid.Cell msg
subtitle tr =
    cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ t tr T.UserTabCaption ] ]


enterName :
    Model
    -> Store s
    -> { a | translator : Translator.Translator }
    -> Material.Grid.Cell Msg
enterName model mdl tr =
    formInput 10 model mdl tr .fullName "fullname" T.EnterName SetFullName


enterUsername :
    Model
    -> Store s
    -> { a | translator : Translator.Translator }
    -> Material.Grid.Cell Msg
enterUsername model mdl tr =
    formInput 20 model mdl tr .name "username" T.EnterUsername SetName


enterPassword :
    Model
    -> Store s
    -> { a | translator : Translator.Translator }
    -> Material.Grid.Cell Msg
enterPassword model mdl _ =
    passInput 30
        model
        mdl
        "pwd"
        .pass
        SetPass
        .passAgain
        SetPassAgain
        ("Choose a password to keep your account safe." ++ model.user.passHash)
        """
          Enter the same password twice, so that it can be checked for typing
          errors. A good password will contain a mixture of letters, numbers and
          punctuation. Should be at least eight characters long, and should be
          changed at regular intervals
        """



-- , formInput 50 model .hostname "hostname" "What is the name of this computer?" SetHostname
-- , formCaption
--     """
--   This name will be used if you make the computer visible to others on
--   the network.
-- """
-- , passInput 60
--     model
--     "admpwd"
--     model.rootPass
--     SetRootPassword
--     model.rootPassAgain
--     SetRootPasswordAgain
--     "Choose a password for the administrator account."
--     """
-- Enter the same password twice, so that it can be checked for
-- typing errors.
-- """


passInput :
    Int
    -> { b | user : a }
    -> Store s
    -> String
    -> (a -> String)
    -> (String -> Msg)
    -> (a -> String)
    -> (String -> Msg)
    -> String
    -> String
    -> Material.Grid.Cell Msg
passInput n model mdl key acc1 event1 acc2 event2 label caption =
    let
        value1 =
            acc1 model.user

        value2 =
            acc2 model.user
    in
    cell [ fullWidth ]
        [ grid []
            [ cell [ fullWidth ]
                [ Options.styled p [ Typo.body2 ] [ text label ] ]
            , cell [ Material.Grid.size All 6 ]
                [ Textfield.render Mdl
                    [ n ]
                    mdl
                    [ Textfield.label "Enter password"
                    , Textfield.floatingLabel
                    , Textfield.password
                    , Textfield.value value1
                    , Options.onInput event1
                    , Options.id key
                    ]
                    []
                ]
            , cell [ Material.Grid.size All 6 ]
                [ Textfield.render Mdl
                    [ n + 1 ]
                    mdl
                    [ Textfield.label "Repeat password"
                    , Textfield.floatingLabel
                    , Textfield.password
                    , Textfield.value value2
                    , Options.onInput event2
                    , Options.id (key ++ "again")
                    , Textfield.error "Passwords don't match"
                        |> Options.when (value1 /= value2)
                    ]
                    []
                ]
            , formCaption caption
            ]
        ]


formInput :
    Int
    -> { c | user : b }
    -> Store s
    -> { a | translator : Translator.Translator }
    -> (b -> String)
    -> String
    -> T.T
    -> (String -> Msg)
    -> Material.Grid.Cell Msg
formInput n model mdl tr acc id label event =
    let
        value =
            acc model.user
    in
    cell [ fullWidth ]
        [ grid []
            [ cell [ Material.Grid.size All 6 ]
                [ Textfield.render Mdl
                    [ n ]
                    mdl
                    [ Textfield.label (tt tr label)
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
