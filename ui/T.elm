module T exposing (..)

import App.Types exposing (Language)
import Dict
import Html
import Translations exposing (Translations)
import Translator exposing (Translator)


-- All possible translation keys


type T
    = Language
    | LanguageTitle
    | LanguageCaption
    | Timezone
    | Keyboard
    | Location
    | Overlay
    | Users
    | Partition
    | NixosOptions
    | NixosOptionsTitle
    | NixosOptionsDescription
    | NixosManual
    | InstallerTitle
    | UserTab
    | UserTabCaption
    | EnterName
    | EnterUsername
    | EnterPassword
    | EnterPasswordCaption
    | EnterHostname
    | EnterHostnameCaption
    | EnterRootPassword
    | EnterRootPasswordCaption
    | SaveConfiguration
    | PerformInstall


defaultLanguage : Language
defaultLanguage =
    en_US


defaultTranslation : Translations
defaultTranslation =
    defaultLanguage.translation


en_CA : Language
en_CA =
    language "English (Canada)" "en_CA.UTF-8" "America/Toronto" english


en_GB : Language
en_GB =
    language "English (Britain)" "en_GB.UTF-8" "Europe/London" english


en_US : Language
en_US =
    language "English (USA)" "en_US.UTF-8" "America/New_York" english


cs_CZ : Language
cs_CZ =
    language "Český" "cs_CZ.UTF-8" "Europe/Prague" czech


de_DE : Language
de_DE =
    language "Deutsch" "de_DE.UTF-8" "Europe/Berlin" german


de_AT : Language
de_AT =
    language "Deutsch (Österreich)" "de_AT.UTF-8" "Europe/Vienna" german


el_GR : Language
el_GR =
    language "Ελληνικά" "el_GR.UTF-8" "Europe/Athens" greek


es_ES : Language
es_ES =
    language "Español" "es_ES.UTF-8" "Europe/Madrid" spanish


fr_FR : Language
fr_FR =
    language "Français" "fr_FR.UTF-8" "Europe/Paris" french


hr_HR : Language
hr_HR =
    language "Hrvatski" "hr_HR.UTF-8" "Europe/Zagreb" croatian


hu_HU : Language
hu_HU =
    language "Magyar" "hu_HU.UTF-8" "Europe/Budapest" hungarian


it_IT : Language
it_IT =
    language "Italiano" "it_IT.UTF-8" "Europe/Rome" italian


ja_JP : Language
ja_JP =
    language "日本語" "ja_JP.UTF-8" "Asia/Tokyo" japanese


ko_KR : Language
ko_KR =
    language "한국어" "ko_KR.UTF-8" "Asia/Seoul" korean


pl_PL : Language
pl_PL =
    language "Polski" "pl_PL.UTF-8" "Europe/Warsaw" polish


pt_BR : Language
pt_BR =
    language "Português (Brasil)" "pt_BR.UTF-8" "America/Sao_Paulo" portuguese


ru_RU : Language
ru_RU =
    language "Русский" "ru_RU.UTF-8" "Europe/Moscow" russian


sr_RS : Language
sr_RS =
    language "Српски (Ћирилица)" "sr_RS.UTF-8" "Europe/Belgrade" serbian


zh_CN : Language
zh_CN =
    language "简体中文" "zh_CN.UTF-8" "Asia/Shanghai" chinese


languages : List Language
languages =
    [ en_CA
    , en_GB
    , en_US
    , cs_CZ
    , de_DE
    , de_AT
    , el_GR
    , es_ES
    , fr_FR
    , hr_HR
    , hu_HU
    , it_IT
    , ja_JP
    , ko_KR
    , pl_PL
    , pt_BR
    , ru_RU
    , sr_RS
    , zh_CN
    ]



-- The fallback translation where the value equals the key, formatted to stand
-- out more, unfortunately the lack of reflection in Elm prevents us from just
-- taking T and turning it into such a Dict, but it's better than having dots
-- all over the place.


fallback : Translations
fallback =
    Dict.fromList
        (List.map (\lit -> ( lit, "[-" ++ lit ++ "-]" ))
            [ "Language"
            , "Timezone"
            , "Keyboard"
            , "Location"
            , "Overlay"
            , "Users"
            , "Partition"
            , "NixosOptions"
            , "NixosOptionsTitle"
            , "NixosOptionsDescription"
            , "NixosManual"
            , "InstallerTitle"
            , "UserTab"
            , "UserTabCaption"
            , "EnterName"
            , "EnterUsername"
            , "EnterPassword"
            , "EnterPasswordCaption"
            , "EnterHostname"
            , "EnterHostnameCaption"
            , "EnterRootPassword"
            , "EnterRootPasswordCaption"
            , "SaveConfiguration"
            , "PerformInstall"
            , "LanguageCaption"
            , "LanguageTitle"
            ]
        )


chinese : Translations
chinese =
    translation []


croatian : Translations
croatian =
    translation []


czech : Translations
czech =
    translation []


english : Translations
english =
    translation
        [ "Language" => "Language"
        , "Location" => "Location"
        , "Timezone" => "Timezone"
        , "Keyboard" => "Keyboard"
        , "Partition" => "Partition"
        , "Users" => "Users"
        , "Overlay" => "Overlay"
        , "LanguageCaption" => "Please select your language and continue to the next step."
        , "LanguateTitle" => "Configure Language"
        ]


french : Translations
french =
    translation []


german : Translations
german =
    translation
        [ "Language" => "Sprache"
        , "Location" => "Region"
        , "Keyboard" => "Eingabe"
        ]


greek : Translations
greek =
    translation []


hungarian : Translations
hungarian =
    translation []


italian : Translations
italian =
    translation []


japanese : Translations
japanese =
    translation
        [ "Language" => "言語"
        , "Location" => "場所"
        , "Keyboard" => "キーボード"
        ]


korean : Translations
korean =
    translation []


polish : Translations
polish =
    translation []


portuguese : Translations
portuguese =
    translation []


russian : Translations
russian =
    translation []


serbian : Translations
serbian =
    translation []


spanish : Translations
spanish =
    translation []


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


translation : List ( comparable, v ) -> Dict.Dict comparable v
translation =
    Dict.fromList


t : { a | translator : Translator } -> T -> Html.Html msg
t model literal =
    Translator.text model.translator literal


tt : { a | translator : Translator } -> T -> String
tt model literal =
    Translator.trans literal model.translator


language : String -> String -> String -> Translations -> Language
language name locale timezone translation =
    { name = name
    , locale = locale
    , translation = translation
    , timezone = timezone
    }
