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


view : Model -> Html Msg
view model =
    grid []
        ([ title model
         , subtitle model
         ]
            ++ List.map (\h -> h model)
                [ enterName, enterUsername, enterPassword ]
        )


title : Model -> Material.Grid.Cell Msg
title tr =
    cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ t tr T.UserTab ] ]


subtitle : Model -> Material.Grid.Cell Msg
subtitle tr =
    cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ t tr T.UserTabCaption ] ]


enterName : Model -> Material.Grid.Cell Msg
enterName model =
    formInput 10 model .fullName "fullname" T.EnterName SetFullName


enterUsername : Model -> Material.Grid.Cell Msg
enterUsername model =
    formInput 20 model .name "username" T.EnterUsername SetName


enterPassword : Model -> Material.Grid.Cell Msg
enterPassword model =
    passInput 30
        model
        model.mdl
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
    ->
        { b
            | mdl : Store s
            , translator : Translator.Translator
            , user : a
        }
    -> (a -> String)
    -> String
    -> T.T
    -> (String -> Msg)
    -> Material.Grid.Cell Msg
formInput n model acc id label event =
    let
        value =
            acc model.user
    in
    cell [ fullWidth ]
        [ grid []
            [ cell [ Material.Grid.size All 6 ]
                [ Textfield.render Mdl
                    [ n ]
                    model.mdl
                    [ Textfield.label (tt model label)
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
