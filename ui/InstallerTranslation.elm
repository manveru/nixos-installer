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
    usEnglishLanguage


defaultTranslation : Translations
defaultTranslation =
    defaultLanguage.translation


type alias Language =
    { name : String
    , locale : String
    , translation : Translations
    , timezone : String
    }


usEnglishLanguage : Language
usEnglishLanguage =
    { name = "English (US)"
    , locale = "en_US.UTF-8"
    , translation = english
    , timezone = "America/Montevideo"
    }


germanLanguage : Language
germanLanguage =
    { name = "Deutsch (Deutschland)"
    , locale = "de_DE.UTF-8"
    , translation = german
    , timezone = "Europe/Berlin"
    }


austrianGermanLanguage : Language
austrianGermanLanguage =
    { germanLanguage
        | name = "Deutsch (Österreich)"
        , locale = "de_AT.UTF-8"
        , timezone = "Europe/Vienna"
    }


japaneseLanguage : Language
japaneseLanguage =
    { name = "日本語"
    , locale = "ja_JP.UTF-8"
    , translation = japanese
    , timezone = "Asia/Tokyo"
    }


languages : List Language
languages =
    [ usEnglishLanguage
    , germanLanguage
    , austrianGermanLanguage
    , japaneseLanguage
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
