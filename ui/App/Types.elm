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
    , timezone : Timezone
    , timezones : List Timezone
    , disk : Disk
    , disks : List Disk
    , language : Language
    , config : Dict.Dict String ConfigOption
    , error : String
    }


type Msg
    = SelectTab Int
    | UrlChange Navigation.Location
    | Mdl (Material.Msg Msg)
      -- Language
    | ChooseLanguage String
    | UpdateTranslator Translations
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
      -- Option
    | InitNixosOptions (Result Http.Error (Dict.Dict String ConfigOption))


type ConfigOption
    = IntConfig { default : Int, example : Maybe Int }
    | NullOrIntConfig { default : Maybe Int, example : Maybe Int }
    | StringConfig { default : Maybe String, example : Maybe String }
    | ListOfStringsConfig { default : List String, example : Maybe (List String) }
    | ListOfDictConfig { default : List (Dict.Dict String String), example : Maybe (List (Dict.Dict String String)) }
    | DictConfig { default : Dict.Dict String String, example : Maybe (Dict.Dict String String) }
    | UnspecifiedConfig {}
    | BooleanConfig { default : Bool, example : Maybe Bool }


type alias LsblkDevice =
    { name : String
    , kname : String
    , majorMinor : String
    , fsType : String
    , mountPoint : String
    , label : String
    , uuid : String
    , partType : String
    , partLabel : String
    }


type alias Disk =
    { path : String
    , model : String
    , serial : String
    , size : String
    }


nullDisk : Disk
nullDisk =
    { path = ""
    , model = ""
    , serial = ""
    , size = ""
    }


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
