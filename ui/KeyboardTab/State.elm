module KeyboardTab.State exposing (init, update)

import App.Types exposing (..)
import Dict
import KeyboardTab.Layouts exposing (..)
import Material


init : ( Model, Cmd Msg )
init =
    ( { keyboard = default }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Choose m ->
            let
                keyboard =
                    model.keyboard

                found =
                    findKeyboard m
            in
            ( { model | keyboard = { keyboard | keyboard = found } }, Cmd.none )

        Mdl m ->
            Material.update Mdl m model


findKeyboard : String -> ( Key, KeyboardLayout )
findKeyboard keyboardName =
    let
        found =
            Maybe.withDefault (Tuple.second default)
                (Dict.get keyboardName all)
    in
    ( keyboardName, found )
