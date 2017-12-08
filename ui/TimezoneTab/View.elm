module TimezoneTab.View exposing (view)

import App.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Material.Grid exposing (Device(..), cell, grid, size)
import Material.Options as Options
import Material.Typography as Typo
import Set
import Style exposing (fullWidth)
import TimezoneTab.Map


view : Model -> Html Msg
view model =
    grid []
        (List.map (\h -> h model)
            [ title, subtitle, chooseRegion, chooseCity, viewMap ]
        )


title : Model -> Material.Grid.Cell msg
title _ =
    cell [ fullWidth ]
        [ Options.styled p [ Typo.display1 ] [ text "Configure Location" ] ]


subtitle : Model -> Material.Grid.Cell msg
subtitle _ =
    cell [ fullWidth ]
        [ Options.styled p [ Typo.subhead ] [ text """
        Select your location, so that the system can use appropriate display
        conventions for your country and set the clock to the correct local
        time.
        """ ]
        ]


chooseRegion : Model -> Material.Grid.Cell Msg
chooseRegion model =
    cell [ Material.Grid.size All 2 ]
        [ select [ onInput ChooseRegion, id "tz-region", name "tz-region" ]
            (List.map (timezoneRegionItem model.timezone.region)
                (timezoneRegions model.timezones)
            )
        ]


chooseCity : Model -> Material.Grid.Cell Msg
chooseCity model =
    if model.timezone.region == "" then
        cell [] []
    else
        cell [ Material.Grid.size All 10 ]
            [ select
                [ onInput ChooseCity
                , id "timezone-city"
                , name "timezone-city"
                ]
                (List.map (timezoneCityItem model.timezone.city)
                    (List.filter
                        (\zone -> zone.region == model.timezone.region)
                        model.timezones
                    )
                )
            ]


viewMap : Model -> Material.Grid.Cell Msg
viewMap model =
    cell [ fullWidth ] [ TimezoneTab.Map.zones model.timezone ]


timezoneRegionItem : String -> String -> Html msg
timezoneRegionItem current region =
    option
        [ Html.Attributes.value region
        , selected (current == region)
        ]
        [ text region ]


timezoneCityItem : String -> Timezone -> Html msg
timezoneCityItem current timezone =
    option
        [ Html.Attributes.value timezone.city
        , selected (current == timezone.city)
        ]
        [ text
            (if timezone.comment == "" then
                timezone.city
             else
                timezone.city ++ " (" ++ timezone.comment ++ ")"
            )
        ]


timezoneRegions : List { a | region : comparable } -> List comparable
timezoneRegions timezones =
    Set.toList (Set.fromList (List.map .region timezones))
