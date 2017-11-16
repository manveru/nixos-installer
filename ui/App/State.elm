module App.State exposing (init, update)

import App.Types exposing (..)
import Material
import Material.Layout
import Navigation exposing (Location)
import T
import Translator exposing (Translator)


init : List Location -> ( Model, Cmd Msg )
init history =
    ( { history = history
      , mdl = Material.Layout.setTabsWidth 600 Material.model
      , selectedTab = 0
      , translator = initTranslator
      , keyboard = nullKeyboard
      , user = nullUser
      , users = []
      , language = nullLanguage
      , timezone = nullTimezone
      , timezones = []
      , disk = nullDisk
      , disks = []
      }
    , Cmd.none
    )


initTranslator : Translator
initTranslator =
    Translator.updateTranslations T.defaultTranslation
        (Translator.makeDefaultTranslator T.fallback)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
        |> updateHistory msg
        -- |> updateMdl msg
        |> updateTab msg
        |> updateTranslator msg
        |> updateKeyboard msg
        |> updateUser msg
        |> updateUsers msg
        |> updateLanguage msg
        |> updateTimezone msg
        |> updateTimezones msg
        |> updateDisk msg
        |> updateDisks msg
    , updateCmd msg model
    )


updateCmd : Msg -> Model -> Cmd Msg
updateCmd msg model =
    case msg of
        _ ->
            Cmd.none


updateHistory : Msg -> Model -> Model
updateHistory msg model =
    { model
        | history =
            case msg of
                UrlChange location ->
                    location :: model.history

                _ ->
                    model.history
    }


updateMdl : Msg -> Model -> ( Model, Cmd Msg )
updateMdl msg model =
    case msg of
        Mdl msg ->
            Material.update Mdl msg model

        _ ->
            ( model, Cmd.none )


updateTab : Msg -> Model -> Model
updateTab msg model =
    { model
        | selectedTab =
            case msg of
                SelectTab index ->
                    index

                _ ->
                    model.selectedTab
    }


updateTranslator : Msg -> Model -> Model
updateTranslator msg model =
    { model
        | translator =
            case msg of
                ChooseTranslator translator ->
                    Translator.updateTranslations
                        model.language.translation
                        model.translator

                _ ->
                    model.translator
    }


updateKeyboard : Msg -> Model -> Model
updateKeyboard msg model =
    model


updateUser : Msg -> Model -> Model
updateUser msg model =
    model


updateUsers : Msg -> Model -> Model
updateUsers msg model =
    model


updateLanguage : Msg -> Model -> Model
updateLanguage msg model =
    model


updateTimezone : Msg -> Model -> Model
updateTimezone msg model =
    model


updateTimezones : Msg -> Model -> Model
updateTimezones msg model =
    model


updateDisk : Msg -> Model -> Model
updateDisk msg model =
    model


updateDisks : Msg -> Model -> Model
updateDisks msg model =
    model
