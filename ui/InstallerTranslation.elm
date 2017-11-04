module InstallerTranslation
    exposing
        ( Language
        , languages
        , defaultLanguage
        , defaultTranslation
        )

import I18Next exposing (Translations, initialTranslations, decodeTranslations)
import Json.Decode exposing (decodeString)


defaultLanguage : Language
defaultLanguage =
    en_US


defaultTranslation : Translations
defaultTranslation =
    defaultLanguage.translation


type alias Language =
    { name : String
    , locale : String
    , translation : Translations
    , timezone : String
    }


en_CA : Language
en_CA =
    { name = "English (Canada)", locale = "en_CA.UTF-8", translation = english, timezone = "America/Montevideo" }


en_GB : Language
en_GB =
    { name = "English (Britain)", locale = "en_GB.UTF-8", translation = english, timezone = "America/Montevideo" }


en_US : Language
en_US =
    { name = "English (USA)", locale = "en_US.UTF-8", translation = english, timezone = "America/Montevideo" }


cs_CZ : Language
cs_CZ =
    { name = "Český", locale = "cs_CZ.UTF-8", translation = english, timezone = "America/Montevideo" }


de_DE : Language
de_DE =
    { name = "Deutsch", locale = "de_DE.UTF-8", translation = german, timezone = "Europe/Berlin" }


de_AT : Language
de_AT =
    { name = "Deutsch (Österreich)", locale = "de_AT.UTF-8", translation = german, timezone = "Europe/Vienna" }


el_GR : Language
el_GR =
    { name = "Ελληνικά", locale = "el_GR.UTF-8", translation = english, timezone = "America/Montevideo" }


es_ES : Language
es_ES =
    { name = "Español", locale = "es_ES.UTF-8", translation = english, timezone = "America/Montevideo" }


fr_FR : Language
fr_FR =
    { name = "Français", locale = "fr_FR.UTF-8", translation = english, timezone = "America/Montevideo" }


hr_HR : Language
hr_HR =
    { name = "Hrvatski", locale = "hr_HR.UTF-8", translation = english, timezone = "America/Montevideo" }


hu_HU : Language
hu_HU =
    { name = "Magyar", locale = "hu_HU.UTF-8", translation = english, timezone = "America/Montevideo" }


it_IT : Language
it_IT =
    { name = "Italiano", locale = "it_IT.UTF-8", translation = english, timezone = "America/Montevideo" }


ja_JP : Language
ja_JP =
    { name = "日本語", locale = "ja_JP.UTF-8", translation = japanese, timezone = "Asia/Tokyo" }


ko_KR : Language
ko_KR =
    { name = "한국어", locale = "ko_KR.UTF-8", translation = english, timezone = "America/Montevideo" }


pl_PL : Language
pl_PL =
    { name = "Polski", locale = "pl_PL.UTF-8", translation = english, timezone = "America/Montevideo" }


pt_BR : Language
pt_BR =
    { name = "Português (Brasil)", locale = "pt_BR.UTF-8", translation = english, timezone = "America/Montevideo" }


ru_RU : Language
ru_RU =
    { name = "Русский", locale = "ru_RU.UTF-8", translation = english, timezone = "America/Montevideo" }


sr_RS : Language
sr_RS =
    { name = "Српски (Ћирилица)", locale = "sr_RS.UTF-8", translation = english, timezone = "America/Montevideo" }


zh_CN : Language
zh_CN =
    { name = "简体中文", locale = "zh_CN.UTF-8", translation = english, timezone = "America/Montevideo" }


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


translate : String -> Translations
translate json =
    Result.withDefault initialTranslations
        (Json.Decode.decodeString decodeTranslations json)


english : Translations
english =
    translate """
{
  "language": "Language",
  "location": "Location",
  "keyboard": "Keyboard",
  "hallo": "Hallo",
  "greetings": {
    "goodDay": "Good Day.",
    "greetName": "Hi {{name}}"
  }
}
"""


german : Translations
german =
    translate """
{
  "language": "Sprache",
  "location": "Region",
  "keyboard": "Eingabe",
  "hallo": "Hallo",
  "greetings": {
    "goodDay": "Good Day.",
    "greetName": "Hi {{name}}"
  }
}
"""


japanese : Translations
japanese =
    translate """
{
  "language": "言語",
  "location": "場所",
  "keyboard": "キーボード",
  "hallo": "こんにちは",
  "greetings": {
    "goodDay": "Good Day.",
    "greetName": "Hi {{name}}"
  }
}
"""
