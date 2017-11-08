module T exposing (..)

import Dict
import Translations exposing (Translations)


type T
    = Language
    | Keyboard
    | Location
    | Overlay
    | Users
    | Partition


defaultTranslation : Translations
defaultTranslation =
    Dict.fromList
        (List.map (\lit -> ( lit, "[-" ++ lit ++ "-]" ))
            [ "Language"
            , "Location"
            , "Keyboard"
            , "Partition"
            , "Users"
            , "Overlay"
            ]
        )
