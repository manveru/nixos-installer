module InstallerTranslation exposing (..)

import I18Next
    exposing
        ( Translations
        , initialTranslations
        , decodeTranslations
        )
import Json.Decode exposing (decodeString)


type alias Language =
    { name : String, locale : String, translation : Translations }


usEnglishLanguage : Language
usEnglishLanguage =
    { name = "English (US)", locale = "en_US.UTF-8", translation = english }


austrianGermanLanguage : Language
austrianGermanLanguage =
    { name = "Deutsch (Österreich)", locale = "de_AT.UTF-8", translation = german }


japaneseLanguage : Language
japaneseLanguage =
    { name = "日本語", locale = "ja_JP.UTF-8", translation = japanese }


languages : List Language
languages =
    [ usEnglishLanguage, austrianGermanLanguage, japaneseLanguage ]


english : Translations
english =
    Result.withDefault initialTranslations
        (Json.Decode.decodeString decodeTranslations
            """
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
        )


german : Translations
german =
    Result.withDefault initialTranslations
        (Json.Decode.decodeString decodeTranslations
            """
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
        )


japanese : Translations
japanese =
    Result.withDefault initialTranslations
        (Json.Decode.decodeString decodeTranslations
            """
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
        )
