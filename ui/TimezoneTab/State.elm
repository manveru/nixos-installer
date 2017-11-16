module TimezoneTab.State exposing (init, update)

import App.Types exposing (..)
import TimezoneTab.Rest exposing (getTimezones)


init =
    ( { timezones = []
      , timezone = nullTimezone
      }
    , Cmd.batch [ getTimezones ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitTimezones (Ok timezones) ->
            let
                timezone =
                    findTimezone timezones "Europe/Berlin"
            in
            ( { model
                | timezones = List.sortBy .name timezones
                , timezone = timezone
              }
            , Cmd.none
            )

        InitTimezones (Err _) ->
            ( model, Cmd.none )

        ChooseTimezone timezoneName ->
            ( { model | timezone = findTimezone model.timezones timezoneName }
            , Cmd.none
            )

        ChooseRegion region ->
            ( { model | timezone = findRegionTimezone model.timezones region }, Cmd.none )

        ChooseCity city ->
            ( { model
                | timezone =
                    findTimezoneByRegionAndCity model.timezones model.timezone.region city
              }
            , Cmd.none
            )


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
