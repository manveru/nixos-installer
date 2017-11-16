module App.Types exposing (..)

import Dict
import Http
import Material
import Material.Textfield as Textfield
import Navigation
import Translations exposing (Translations)
import Translator exposing (Translator)


type alias Model =
    { history : List Navigation.Location
    , mdl : Material.Model
    , selectedTab : Int
    , translator : Translator
    , keyboard : Keyboard
    , user : User
    , users : List User
    , language : Language
    , timezone : Timezone
    , timezones : List Timezone
    , disk : Disk
    , disks : List Disk
    , language : Language
    }


type Msg
    = SelectTab Int
    | UrlChange Navigation.Location
    | Mdl (Material.Msg Msg)
      -- Language
    | ChooseLanguage String
    | ChooseTranslator Translator
      -- Keyboard
    | ChooseKeyboard String
      -- User
    | AddUser User
    | NewUser
    | SetName String
    | SetFullName String
    | SetPass String
    | SetPassHash String
    | SetPassAgain String
      -- Timezone
    | InitTimezones (Result Http.Error (List Timezone))
    | ChooseTimezone String
    | ChooseRegion String
    | ChooseCity String
      -- Partition
    | InitPartitions (Result Http.Error (List Disk))
    | ChoosePartition String


type alias Disk =
    { path : String, model : String, serial : String, size : String }


nullDisk : Disk
nullDisk =
    { path = "", model = "", serial = "", size = "" }


type alias Timezone =
    { country : String
    , coords : String
    , region : String
    , city : String
    , name : String
    , comment : String
    }


nullTimezone : Timezone
nullTimezone =
    { country = ""
    , coords = ""
    , region = ""
    , city = ""
    , name = ""
    , comment = ""
    }


type alias Language =
    { name : String
    , locale : String
    , translation : Translations
    , timezone : String
    }


nullLanguage : Language
nullLanguage =
    { name = ""
    , locale = ""
    , translation = Dict.fromList []
    , timezone = ""
    }


type alias Keyboard =
    { keyboard : ( Key, KeyboardLayout )
    }


nullKeyboard : Keyboard
nullKeyboard =
    { keyboard = ( "us", { layout = "us", variant = "", name = "English (US)" } )
    }


type alias Key =
    String


type alias KeyboardLayout =
    { layout : String
    , variant : String
    , name : String
    }


nullKeyboardLayout : KeyboardLayout
nullKeyboardLayout =
    { layout = "en"
    , variant = ""
    , name = "English (US)"
    }


type alias User =
    { name : String
    , fullName : String
    , pass : String
    , passHash : String
    , passAgain : String
    }


nullUser : User
nullUser =
    { name = ""
    , fullName = ""
    , pass = ""
    , passHash = ""
    , passAgain = ""
    }


type alias Index =
    List Int


type alias Indexed x =
    Dict.Dict Index x


type alias Store s =
    { s | textfield : Indexed Textfield.Model }
