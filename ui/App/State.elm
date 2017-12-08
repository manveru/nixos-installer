module App.State exposing (init, update)

import App.Types exposing (..)
import Dict
import Material
import Material.Layout
import Navigation exposing (Location)
import NixosOptions
import T
import TimezoneTab.Rest
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
      , config = nullConfig
      , error = ""
      }
    , Cmd.batch
        [ TimezoneTab.Rest.getTimezones
        , NixosOptions.getNixosOptions
        ]
    )


nullConfig : Dict.Dict String ConfigOption
nullConfig =
    Dict.empty


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
        |> updateKeyboard msg
        |> updateUser msg
        |> updateUsers msg
        |> updateLanguage msg
        |> updateTranslator msg
        |> updateTimezones msg
        |> updateTimezone msg
        |> updateDisk msg
        |> updateDisks msg
        |> updateOptions msg
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
                ChooseLanguage _ ->
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
    { model
        | language =
            case msg of
                ChooseLanguage name ->
                    findLanguage name

                _ ->
                    model.language
    }


findLanguage : String -> Language
findLanguage name =
    Maybe.withDefault T.defaultLanguage
        (List.head (List.filter (\lang -> lang.name == name) T.languages))


updateTimezone : Msg -> Model -> Model
updateTimezone msg model =
    { model
        | timezone =
            case msg of
                ChooseLanguage name ->
                    findTimezone model.timezones model.language.timezone

                ChooseTimezone timezoneName ->
                    findTimezone model.timezones timezoneName

                ChooseRegion region ->
                    findRegionTimezone model.timezones region

                ChooseCity city ->
                    findTimezoneByRegionAndCity model.timezones model.timezone.region city

                _ ->
                    model.timezone
    }


findRegionTimezone : List Timezone -> String -> Timezone
findRegionTimezone timezones regionName =
    Maybe.withDefault nullTimezone
        (List.head (List.filter (\timezone -> timezone.region == regionName) timezones))


findTimezoneByRegionAndCity : List Timezone -> String -> String -> Timezone
findTimezoneByRegionAndCity timezones region city =
    Maybe.withDefault nullTimezone
        (List.head
            (List.filter
                (\timezone ->
                    (timezone.region == region) && (timezone.city == city)
                )
                timezones
            )
        )


findTimezone : List Timezone -> String -> Timezone
findTimezone timezones timezoneName =
    Maybe.withDefault nullTimezone
        (List.head (List.filter (\timezone -> timezone.name == timezoneName) timezones))


updateTimezones : Msg -> Model -> Model
updateTimezones msg model =
    { model
        | timezones =
            case msg of
                InitTimezones (Ok zones) ->
                    List.sortBy .name zones

                InitTimezones (Err e) ->
                    model.timezones

                _ ->
                    model.timezones
    }


updateDisk : Msg -> Model -> Model
updateDisk msg model =
    model


updateDisks : Msg -> Model -> Model
updateDisks msg model =
    model


updateOptions : Msg -> Model -> Model
updateOptions msg model =
    case msg of
        InitNixosOptions (Ok options) ->
            { model | config = options }

        InitNixosOptions (Err e) ->
            { model | error = toString e }

        _ ->
            model
