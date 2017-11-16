module LanguageTab.State exposing (init, update)

import LanguageTab.Types exposing (..)
import T


init : ( Model, Cmd Msg )
init =
    ( { language = T.defaultLanguage
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Choose name ->
            ( { model | language = findLanguage name }, Cmd.none )


findLanguage : String -> Language
findLanguage name =
    Maybe.withDefault T.defaultLanguage
        (List.head (List.filter (\lang -> lang.name == name) T.languages))
