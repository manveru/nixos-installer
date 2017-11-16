module App.State exposing (update)

import App.Types exposing (..)
import KeyboardTab.Layouts
import Material
import Material.Layout
import T
import Translator exposing (Translator)


init : ( Model, Cmd Msg )
init =
    { mdl = Material.Layout.setTabsWidth 600 Material.model
    , selectedTab = 0
    , translator = initTranslator
    , keyboard = KeyboardTab.Layouts.default
    , user = nullUser
    , users = []
    , language = T.defaultTranslation
    , timezone = nullTimezone
    , timezones = []
    , disk = nullDisk
    , disks = []
    }


initTranslator =
    Translator.updateTranslations T.defaultTranslation
        (Translator.makeDefaultTranslator T.fallback)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( { model
        | history = updateHistory msg model
        , mdl = updateMdl msg model
        , selectedTab = updateTab msg model
        , translator = updateTranslator msg model
        , keyboard = updateKeyboard msg model
        , user = updateUser msg model
        , users = updateUsers msg model
        , language = updateLanguage msg model
        , timezone = updateTimezone msg model
        , timezones = updateTimezones msg model
        , disk = updateDisk msg model
        , disks = updateDisks msg model
      }
    , Cmd.none
    )


updateHistory msg model =
    case msg of
        UrlChange location ->
            location :: model.history


updateMdl msg model =
    case msg of
        Mdl msg ->
            Material.update Mdl msg model

        _ ->
            model.mdl


updateTab msg model =
    case msg of
        SelectTab index ->
            index


updateTranslator msg model =
    case msg of
        ChooseTranslator translator ->
            Translator.updateTranslations
                model.language.translation
                model.translator


updateKeyboard msg model =
    nullKeyboard


updateUser msg model =
    nullUser


updateUsers msg model =
    []


updateLanguage msg model =
    nullLanguage


updateTimezone msg model =
    nullTimezone


updateTimezones msg model =
    []


updateDisk msg model =
    nullDisk


updateDisks msg model =
    []
