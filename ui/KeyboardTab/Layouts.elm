module KeyboardTab.Layouts exposing (all, default)

import App.Types exposing (..)
import Dict


default : ( Key, KeyboardLayout )
default =
    ( "us", { layout = "us", variant = "", name = "English (US)" } )


all : Dict.Dict Key KeyboardLayout
all =
    Dict.fromList
        (List.map
            (\entry ->
                let
                    ( key, ( layout, variant, name ) ) =
                        entry
                in
                ( key, { layout = layout, variant = variant, name = name } )
            )
            data
        )



-- Extracted from
-- https://github.com/hsgg/lxpanel/blob/a04979c2878aff29adcd9b3fa2ba74a4ea436d47/src/plugins/xkb/xkeyboardconfig/layouts.cfg


data : List ( String, ( String, String, String ) )
data =
    [ ( "ad", ( "ad", "", "Catalan" ) )
    , ( "af", ( "af", "", "Afghani" ) )
    , ( "af,fa-olpc", ( "af", "fa-olpc", "Persian (Afghanistan, Dari OLPC)" ) )
    , ( "af,olpc-ps", ( "af", "olpc-ps", "Pashto (Afghanistan, OLPC)" ) )
    , ( "af,ps", ( "af", "ps", "Pashto" ) )
    , ( "af,uz", ( "af", "uz", "Uzbek (Afghanistan)" ) )
    , ( "af,uz-olpc", ( "af", "uz-olpc", "Uzbek (Afghanistan, OLPC)" ) )
    , ( "al", ( "al", "", "Albanian" ) )
    , ( "am", ( "am", "", "Armenian" ) )
    , ( "am,eastern", ( "am", "eastern", "Armenian (eastern)" ) )
    , ( "am,eastern-alt", ( "am", "eastern-alt", "Armenian (alternative eastern)" ) )
    , ( "am,phonetic", ( "am", "phonetic", "Armenian (phonetic)" ) )
    , ( "am,phonetic-alt", ( "am", "phonetic-alt", "Armenian (alternative phonetic)" ) )
    , ( "am,western", ( "am", "western", "Armenian (western)" ) )
    , ( "ara", ( "ara", "", "Arabic" ) )
    , ( "ara,azerty", ( "ara", "azerty", "Arabic (azerty)" ) )
    , ( "ara,azerty_digits", ( "ara", "azerty_digits", "Arabic (azerty/digits)" ) )
    , ( "ara,buckwalter", ( "ara", "buckwalter", "Arabic (Buckwalter)" ) )
    , ( "ara,digits", ( "ara", "digits", "Arabic (digits)" ) )
    , ( "ara,qwerty", ( "ara", "qwerty", "Arabic (qwerty)" ) )
    , ( "ara,qwerty_digits", ( "ara", "qwerty_digits", "Arabic (qwerty/digits)" ) )
    , ( "at", ( "at", "", "German (Austria)" ) )
    , ( "at,mac", ( "at", "mac", "German (Austria, Macintosh)" ) )
    , ( "at,nodeadkeys", ( "at", "nodeadkeys", "German (Austria, eliminate dead keys)" ) )
    , ( "at,sundeadkeys", ( "at", "sundeadkeys", "German (Austria, Sun dead keys)" ) )
    , ( "az", ( "az", "", "Azerbaijani" ) )
    , ( "az,cyrillic", ( "az", "cyrillic", "Azerbaijani (Cyrillic)" ) )
    , ( "ba", ( "ba", "", "Bosnian" ) )
    , ( "ba,alternatequotes", ( "ba", "alternatequotes", "Bosnian (use guillemets for quotes)" ) )
    , ( "ba,unicode", ( "ba", "unicode", "Bosnian (use Bosnian digraphs)" ) )
    , ( "ba,unicodeus", ( "ba", "unicodeus", "Bosnian (US keyboard with Bosnian digraphs)" ) )
    , ( "ba,us", ( "ba", "us", "Bosnian (US keyboard with Bosnian letters)" ) )
    , ( "bd", ( "bd", "", "Bengali" ) )
    , ( "bd,probhat", ( "bd", "probhat", "Bengali (Probhat)" ) )
    , ( "be", ( "be", "", "Belgian" ) )
    , ( "be,iso-alternate", ( "be", "iso-alternate", "Belgian (ISO alternate)" ) )
    , ( "be,nodeadkeys", ( "be", "nodeadkeys", "Belgian (eliminate dead keys)" ) )
    , ( "be,oss", ( "be", "oss", "Belgian (alternative)" ) )
    , ( "be,oss_latin9", ( "be", "oss_latin9", "Belgian (alternative, latin-9 only)" ) )
    , ( "be,oss_sundeadkeys", ( "be", "oss_sundeadkeys", "Belgian (alternative, Sun dead keys)" ) )
    , ( "be,sundeadkeys", ( "be", "sundeadkeys", "Belgian (Sun dead keys)" ) )
    , ( "be,wang", ( "be", "wang", "Belgian (Wang model 724 azerty)" ) )
    , ( "bg", ( "bg", "", "Bulgarian" ) )
    , ( "bg,bas_phonetic", ( "bg", "bas_phonetic", "Bulgarian (new phonetic)" ) )
    , ( "bg,phonetic", ( "bg", "phonetic", "Bulgarian (traditional phonetic)" ) )
    , ( "br", ( "br", "", "Portuguese (Brazil)" ) )
    , ( "br,dvorak", ( "br", "dvorak", "Portuguese (Brazil, Dvorak)" ) )
    , ( "br,nativo", ( "br", "nativo", "Portuguese (Brazil, nativo)" ) )
    , ( "br,nativo-epo", ( "br", "nativo-epo", "Portuguese (Brazil, nativo for Esperanto)" ) )
    , ( "br,nativo-us", ( "br", "nativo-us", "Portuguese (Brazil, nativo for USA keyboards)" ) )
    , ( "br,nodeadkeys", ( "br", "nodeadkeys", "Portuguese (Brazil, eliminate dead keys)" ) )
    , ( "brai", ( "brai", "", "Braille" ) )
    , ( "brai,left_hand", ( "brai", "left_hand", "Braille (left hand)" ) )
    , ( "brai,right_hand", ( "brai", "right_hand", "Braille (right hand)" ) )
    , ( "bt", ( "bt", "", "Dzongkha" ) )
    , ( "bw", ( "bw", "", "Tswana" ) )
    , ( "by", ( "by", "", "Belarusian" ) )
    , ( "by,latin", ( "by", "latin", "Belarusian (Latin)" ) )
    , ( "by,legacy", ( "by", "legacy", "Belarusian (legacy)" ) )
    , ( "ca", ( "ca", "", "French (Canada)" ) )
    , ( "ca,eng", ( "ca", "eng", "English (Canada)" ) )
    , ( "ca,fr-dvorak", ( "ca", "fr-dvorak", "French (Canada, Dvorak)" ) )
    , ( "ca,fr-legacy", ( "ca", "fr-legacy", "French (Canada, legacy)" ) )
    , ( "ca,ike", ( "ca", "ike", "Inuktitut" ) )
    , ( "ca,multi", ( "ca", "multi", "Canadian Multilingual (first part)" ) )
    , ( "ca,multi-2gr", ( "ca", "multi-2gr", "Canadian Multilingual (second part)" ) )
    , ( "ca,multix", ( "ca", "multix", "Canadian Multilingual" ) )
    , ( "cd", ( "cd", "", "French (Democratic Republic of the Congo)" ) )
    , ( "ch", ( "ch", "", "German (Switzerland)" ) )
    , ( "ch,de_mac", ( "ch", "de_mac", "German (Switzerland, Macintosh)" ) )
    , ( "ch,de_nodeadkeys", ( "ch", "de_nodeadkeys", "German (Switzerland, eliminate dead keys)" ) )
    , ( "ch,de_sundeadkeys", ( "ch", "de_sundeadkeys", "German (Switzerland, Sun dead keys)" ) )
    , ( "ch,fr", ( "ch", "fr", "French (Switzerland)" ) )
    , ( "ch,fr_mac", ( "ch", "fr_mac", "French (Switzerland, Macintosh)" ) )
    , ( "ch,fr_nodeadkeys", ( "ch", "fr_nodeadkeys", "French (Switzerland, eliminate dead keys)" ) )
    , ( "ch,fr_sundeadkeys", ( "ch", "fr_sundeadkeys", "French (Switzerland, Sun dead keys)" ) )
    , ( "ch,legacy", ( "ch", "legacy", "German (Switzerland, legacy)" ) )
    , ( "cm", ( "cm", "", "English (Cameroon)" ) )
    , ( "cm,azerty", ( "cm", "azerty", "Cameroon Multilingual (azerty)" ) )
    , ( "cm,dvorak", ( "cm", "dvorak", "Cameroon Multilingual (Dvorak)" ) )
    , ( "cm,french", ( "cm", "french", "French (Cameroon)" ) )
    , ( "cm,qwerty", ( "cm", "qwerty", "Cameroon Multilingual (qwerty)" ) )
    , ( "cn", ( "cn", "", "Chinese" ) )
    , ( "cn,tib", ( "cn", "tib", "Tibetan" ) )
    , ( "cn,tib_asciinum", ( "cn", "tib_asciinum", "Tibetan (with ASCII numerals)" ) )
    , ( "cn,uig", ( "cn", "uig", "Uyghur" ) )
    , ( "cz", ( "cz", "", "Czech" ) )
    , ( "cz,bksl", ( "cz", "bksl", "Czech (with <> key)" ) )
    , ( "cz,dvorak-ucw", ( "cz", "dvorak-ucw", "Czech (US Dvorak with CZ UCW support)" ) )
    , ( "cz,qwerty", ( "cz", "qwerty", "Czech (qwerty)" ) )
    , ( "cz,qwerty_bksl", ( "cz", "qwerty_bksl", "Czech (qwerty, extended Backslash)" ) )
    , ( "cz,ucw", ( "cz", "ucw", "Czech (UCW layout, accented letters only)" ) )
    , ( "de", ( "de", "", "German" ) )
    , ( "de,deadacute", ( "de", "deadacute", "German (dead acute)" ) )
    , ( "de,deadgraveacute", ( "de", "deadgraveacute", "German (dead grave acute)" ) )
    , ( "de,dsb", ( "de", "dsb", "Lower Sorbian" ) )
    , ( "de,dsb_qwertz", ( "de", "dsb_qwertz", "Lower Sorbian (qwertz)" ) )
    , ( "de,dvorak", ( "de", "dvorak", "German (Dvorak)" ) )
    , ( "de,mac", ( "de", "mac", "German (Macintosh)" ) )
    , ( "de,mac_nodeadkeys", ( "de", "mac_nodeadkeys", "German (Macintosh, eliminate dead keys)" ) )
    , ( "de,neo", ( "de", "neo", "German (Neo 2)" ) )
    , ( "de,nodeadkeys", ( "de", "nodeadkeys", "German (eliminate dead keys)" ) )
    , ( "de,ro", ( "de", "ro", "Romanian (Germany)" ) )
    , ( "de,ro_nodeadkeys", ( "de", "ro_nodeadkeys", "Romanian (Germany, eliminate dead keys)" ) )
    , ( "de,ru", ( "de", "ru", "Russian (Germany, phonetic)" ) )
    , ( "de,sundeadkeys", ( "de", "sundeadkeys", "German (Sun dead keys)" ) )
    , ( "dk", ( "dk", "", "Danish" ) )
    , ( "dk,dvorak", ( "dk", "dvorak", "Danish (Dvorak)" ) )
    , ( "dk,mac", ( "dk", "mac", "Danish (Macintosh)" ) )
    , ( "dk,mac_nodeadkeys", ( "dk", "mac_nodeadkeys", "Danish (Macintosh, eliminate dead keys)" ) )
    , ( "dk,nodeadkeys", ( "dk", "nodeadkeys", "Danish (eliminate dead keys)" ) )
    , ( "ee", ( "ee", "", "Estonian" ) )
    , ( "ee,dvorak", ( "ee", "dvorak", "Estonian (Dvorak)" ) )
    , ( "ee,nodeadkeys", ( "ee", "nodeadkeys", "Estonian (eliminate dead keys)" ) )
    , ( "ee,us", ( "ee", "us", "Estonian (US keyboard with Estonian letters)" ) )
    , ( "epo", ( "epo", "", "Esperanto" ) )
    , ( "epo,legacy", ( "epo", "legacy", "Esperanto (displaced semicolon and quote, obsolete)" ) )
    , ( "es", ( "es", "", "Spanish" ) )
    , ( "es,ast", ( "es", "ast", "Asturian (Spain, with bottom-dot H and bottom-dot L)" ) )
    , ( "es,cat", ( "es", "cat", "Catalan (Spain, with middle-dot L)" ) )
    , ( "es,deadtilde", ( "es", "deadtilde", "Spanish (include dead tilde)" ) )
    , ( "es,dvorak", ( "es", "dvorak", "Spanish (Dvorak)" ) )
    , ( "es,mac", ( "es", "mac", "Spanish (Macintosh)" ) )
    , ( "es,nodeadkeys", ( "es", "nodeadkeys", "Spanish (eliminate dead keys)" ) )
    , ( "es,sundeadkeys", ( "es", "sundeadkeys", "Spanish (Sun dead keys)" ) )
    , ( "et", ( "et", "", "Amharic" ) )
    , ( "fi", ( "fi", "", "Finnish" ) )
    , ( "fi,classic", ( "fi", "classic", "Finnish (classic)" ) )
    , ( "fi,mac", ( "fi", "mac", "Finnish (Macintosh)" ) )
    , ( "fi,nodeadkeys", ( "fi", "nodeadkeys", "Finnish (classic, eliminate dead keys)" ) )
    , ( "fi,smi", ( "fi", "smi", "Northern Saami (Finland)" ) )
    , ( "fo", ( "fo", "", "Faroese" ) )
    , ( "fo,nodeadkeys", ( "fo", "nodeadkeys", "Faroese (eliminate dead keys)" ) )
    , ( "fr", ( "fr", "", "French" ) )
    , ( "fr,bepo", ( "fr", "bepo", "French (Bepo, ergonomic, Dvorak way)" ) )
    , ( "fr,bepo_latin9", ( "fr", "bepo_latin9", "French (Bepo, ergonomic, Dvorak way, latin-9 only)" ) )
    , ( "fr,bre", ( "fr", "bre", "French (Breton)" ) )
    , ( "fr,dvorak", ( "fr", "dvorak", "French (Dvorak)" ) )
    , ( "fr,geo", ( "fr", "geo", "Georgian (France, AZERTY Tskapo)" ) )
    , ( "fr,latin9", ( "fr", "latin9", "French (legacy, alternative)" ) )
    , ( "fr,latin9_nodeadkeys", ( "fr", "latin9_nodeadkeys", "French (legacy, alternative, eliminate dead keys)" ) )
    , ( "fr,latin9_sundeadkeys", ( "fr", "latin9_sundeadkeys", "French (legacy, alternative, Sun dead keys)" ) )
    , ( "fr,mac", ( "fr", "mac", "French (Macintosh)" ) )
    , ( "fr,nodeadkeys", ( "fr", "nodeadkeys", "French (eliminate dead keys)" ) )
    , ( "fr,oci", ( "fr", "oci", "Occitan" ) )
    , ( "fr,oss", ( "fr", "oss", "French (alternative)" ) )
    , ( "fr,oss_latin9", ( "fr", "oss_latin9", "French (alternative, latin-9 only)" ) )
    , ( "fr,oss_nodeadkeys", ( "fr", "oss_nodeadkeys", "French (alternative, eliminate dead keys)" ) )
    , ( "fr,oss_sundeadkeys", ( "fr", "oss_sundeadkeys", "French (alternative, Sun dead keys)" ) )
    , ( "fr,sundeadkeys", ( "fr", "sundeadkeys", "French (Sun dead keys)" ) )
    , ( "gb", ( "gb", "", "English (UK)" ) )
    , ( "gb,colemak", ( "gb", "colemak", "English (UK, Colemak)" ) )
    , ( "gb,dvorak", ( "gb", "dvorak", "English (UK, Dvorak)" ) )
    , ( "gb,dvorakukp", ( "gb", "dvorakukp", "English (UK, Dvorak with UK punctuation)" ) )
    , ( "gb,extd", ( "gb", "extd", "English (UK, extended WinKeys)" ) )
    , ( "gb,intl", ( "gb", "intl", "English (UK, international with dead keys)" ) )
    , ( "gb,mac", ( "gb", "mac", "English (UK, Macintosh)" ) )
    , ( "gb,mac_intl", ( "gb", "mac_intl", "English (UK, Macintosh international)" ) )
    , ( "ge", ( "ge", "", "Georgian" ) )
    , ( "ge,ergonomic", ( "ge", "ergonomic", "Georgian (ergonomic)" ) )
    , ( "ge,mess", ( "ge", "mess", "Georgian (MESS)" ) )
    , ( "ge,os", ( "ge", "os", "Ossetian (Georgia)" ) )
    , ( "ge,ru", ( "ge", "ru", "Russian (Georgia)" ) )
    , ( "gh", ( "gh", "", "English (Ghana)" ) )
    , ( "gh,akan", ( "gh", "akan", "Akan" ) )
    , ( "gh,avn", ( "gh", "avn", "Avatime" ) )
    , ( "gh,ewe", ( "gh", "ewe", "Ewe" ) )
    , ( "gh,fula", ( "gh", "fula", "Fula" ) )
    , ( "gh,ga", ( "gh", "ga", "Ga" ) )
    , ( "gh,generic", ( "gh", "generic", "English (Ghana, multilingual)" ) )
    , ( "gh,gillbt", ( "gh", "gillbt", "English (Ghana, GILLBT)" ) )
    , ( "gh,hausa", ( "gh", "hausa", "Hausa" ) )
    , ( "gn", ( "gn", "", "French (Guinea)" ) )
    , ( "gr", ( "gr", "", "Greek" ) )
    , ( "gr,extended", ( "gr", "extended", "Greek (extended)" ) )
    , ( "gr,nodeadkeys", ( "gr", "nodeadkeys", "Greek (eliminate dead keys)" ) )
    , ( "gr,polytonic", ( "gr", "polytonic", "Greek (polytonic)" ) )
    , ( "gr,simple", ( "gr", "simple", "Greek (simple)" ) )
    , ( "hr", ( "hr", "", "Croatian" ) )
    , ( "hr,alternatequotes", ( "hr", "alternatequotes", "Croatian (use guillemets for quotes)" ) )
    , ( "hr,unicode", ( "hr", "unicode", "Croatian (use Croatian digraphs)" ) )
    , ( "hr,unicodeus", ( "hr", "unicodeus", "Croatian (US keyboard with Croatian digraphs)" ) )
    , ( "hr,us", ( "hr", "us", "Croatian (US keyboard with Croatian letters)" ) )
    , ( "hu", ( "hu", "", "Hungarian" ) )
    , ( "hu,101_qwerty_comma_dead", ( "hu", "101_qwerty_comma_dead", "Hungarian (101/qwerty/comma/dead keys)" ) )
    , ( "hu,101_qwerty_comma_nodead", ( "hu", "101_qwerty_comma_nodead", "Hungarian (101/qwerty/comma/eliminate dead keys)" ) )
    , ( "hu,101_qwerty_dot_dead", ( "hu", "101_qwerty_dot_dead", "Hungarian (101/qwerty/dot/dead keys)" ) )
    , ( "hu,101_qwerty_dot_nodead", ( "hu", "101_qwerty_dot_nodead", "Hungarian (101/qwerty/dot/eliminate dead keys)" ) )
    , ( "hu,101_qwertz_comma_dead", ( "hu", "101_qwertz_comma_dead", "Hungarian (101/qwertz/comma/dead keys)" ) )
    , ( "hu,101_qwertz_comma_nodead", ( "hu", "101_qwertz_comma_nodead", "Hungarian (101/qwertz/comma/eliminate dead keys)" ) )
    , ( "hu,101_qwertz_dot_dead", ( "hu", "101_qwertz_dot_dead", "Hungarian (101/qwertz/dot/dead keys)" ) )
    , ( "hu,101_qwertz_dot_nodead", ( "hu", "101_qwertz_dot_nodead", "Hungarian (101/qwertz/dot/eliminate dead keys)" ) )
    , ( "hu,102_qwerty_comma_dead", ( "hu", "102_qwerty_comma_dead", "Hungarian (102/qwerty/comma/dead keys)" ) )
    , ( "hu,102_qwerty_comma_nodead", ( "hu", "102_qwerty_comma_nodead", "Hungarian (102/qwerty/comma/eliminate dead keys)" ) )
    , ( "hu,102_qwerty_dot_dead", ( "hu", "102_qwerty_dot_dead", "Hungarian (102/qwerty/dot/dead keys)" ) )
    , ( "hu,102_qwerty_dot_nodead", ( "hu", "102_qwerty_dot_nodead", "Hungarian (102/qwerty/dot/eliminate dead keys)" ) )
    , ( "hu,102_qwertz_comma_dead", ( "hu", "102_qwertz_comma_dead", "Hungarian (102/qwertz/comma/dead keys)" ) )
    , ( "hu,102_qwertz_comma_nodead", ( "hu", "102_qwertz_comma_nodead", "Hungarian (102/qwertz/comma/eliminate dead keys)" ) )
    , ( "hu,102_qwertz_dot_dead", ( "hu", "102_qwertz_dot_dead", "Hungarian (102/qwertz/dot/dead keys)" ) )
    , ( "hu,102_qwertz_dot_nodead", ( "hu", "102_qwertz_dot_nodead", "Hungarian (102/qwertz/dot/eliminate dead keys)" ) )
    , ( "hu,nodeadkeys", ( "hu", "nodeadkeys", "Hungarian (eliminate dead keys)" ) )
    , ( "hu,qwerty", ( "hu", "qwerty", "Hungarian (qwerty)" ) )
    , ( "hu,standard", ( "hu", "standard", "Hungarian (standard)" ) )
    , ( "ie", ( "ie", "", "Irish" ) )
    , ( "ie,clogaelach", ( "ie", "clogaelach", "CloGaelach" ) )
    , ( "ie,ogam", ( "ie", "ogam", "Ogham" ) )
    , ( "ie,ogam_is434", ( "ie", "ogam_is434", "Ogham (IS434)" ) )
    , ( "ie,unicodeexpert", ( "ie", "unicodeexpert", "Irish (UnicodeExpert)" ) )
    , ( "il", ( "il", "", "Hebrew" ) )
    , ( "il,biblical", ( "il", "biblical", "Hebrew (Biblical, Tiro)" ) )
    , ( "il,lyx", ( "il", "lyx", "Hebrew (lyx)" ) )
    , ( "il,phonetic", ( "il", "phonetic", "Hebrew (phonetic)" ) )
    , ( "in", ( "in", "", "Indian" ) )
    , ( "in,ben", ( "in", "ben", "Bengali (India)" ) )
    , ( "in,ben_baishakhi", ( "in", "ben_baishakhi", "Bengali (India, Baishakhi)" ) )
    , ( "in,ben_bornona", ( "in", "ben_bornona", "Bengali (India, Bornona)" ) )
    , ( "in,ben_gitanjali", ( "in", "ben_gitanjali", "Bengali (India, Uni Gitanjali)" ) )
    , ( "in,ben_inscript", ( "in", "ben_inscript", "Bengali (India, Baishakhi Inscript)" ) )
    , ( "in,ben_probhat", ( "in", "ben_probhat", "Bengali (India, Probhat)" ) )
    , ( "in,bolnagri", ( "in", "bolnagri", "Hindi (Bolnagri)" ) )
    , ( "in,eng", ( "in", "eng", "English (India, with RupeeSign)" ) )
    , ( "in,guj", ( "in", "guj", "Gujarati" ) )
    , ( "in,guru", ( "in", "guru", "Punjabi (Gurmukhi)" ) )
    , ( "in,hin-wx", ( "in", "hin-wx", "Hindi (Wx)" ) )
    , ( "in,jhelum", ( "in", "jhelum", "Punjabi (Gurmukhi Jhelum)" ) )
    , ( "in,kan", ( "in", "kan", "Kannada" ) )
    , ( "in,mal", ( "in", "mal", "Malayalam" ) )
    , ( "in,mal_enhanced", ( "in", "mal_enhanced", "Malayalam (enhanced Inscript with Rupee Sign)" ) )
    , ( "in,mal_lalitha", ( "in", "mal_lalitha", "Malayalam (Lalitha)" ) )
    , ( "in,ori", ( "in", "ori", "Oriya" ) )
    , ( "in,tam", ( "in", "tam", "Tamil" ) )
    , ( "in,tam_keyboard_with_numerals", ( "in", "tam_keyboard_with_numerals", "Tamil (keyboard with numerals)" ) )
    , ( "in,tam_tab", ( "in", "tam_tab", "Tamil (TAB typewriter)" ) )
    , ( "in,tam_tscii", ( "in", "tam_tscii", "Tamil (TSCII typewriter)" ) )
    , ( "in,tam_unicode", ( "in", "tam_unicode", "Tamil (Unicode)" ) )
    , ( "in,tel", ( "in", "tel", "Telugu" ) )
    , ( "in,urd-phonetic", ( "in", "urd-phonetic", "Urdu (phonetic)" ) )
    , ( "in,urd-phonetic3", ( "in", "urd-phonetic3", "Urdu (alternative phonetic)" ) )
    , ( "in,urd-winkeys", ( "in", "urd-winkeys", "Urdu (WinKeys)" ) )
    , ( "iq", ( "iq", "", "Iraqi" ) )
    , ( "iq,ku", ( "iq", "ku", "Kurdish (Iraq, Latin Q)" ) )
    , ( "iq,ku_alt", ( "iq", "ku_alt", "Kurdish (Iraq, Latin Alt-Q)" ) )
    , ( "iq,ku_ara", ( "iq", "ku_ara", "Kurdish (Iraq, Arabic-Latin)" ) )
    , ( "iq,ku_f", ( "iq", "ku_f", "Kurdish (Iraq, F)" ) )
    , ( "ir", ( "ir", "", "Persian" ) )
    , ( "ir,ku", ( "ir", "ku", "Kurdish (Iran, Latin Q)" ) )
    , ( "ir,ku_alt", ( "ir", "ku_alt", "Kurdish (Iran, Latin Alt-Q)" ) )
    , ( "ir,ku_ara", ( "ir", "ku_ara", "Kurdish (Iran, Arabic-Latin)" ) )
    , ( "ir,ku_f", ( "ir", "ku_f", "Kurdish (Iran, F)" ) )
    , ( "ir,pes_keypad", ( "ir", "pes_keypad", "Persian (with Persian Keypad)" ) )
    , ( "is", ( "is", "", "Icelandic" ) )
    , ( "is,dvorak", ( "is", "dvorak", "Icelandic (Dvorak)" ) )
    , ( "is,mac", ( "is", "mac", "Icelandic (Macintosh)" ) )
    , ( "is,nodeadkeys", ( "is", "nodeadkeys", "Icelandic (eliminate dead keys)" ) )
    , ( "is,sundeadkeys", ( "is", "sundeadkeys", "Icelandic (Sun dead keys)" ) )
    , ( "it", ( "it", "", "Italian" ) )
    , ( "it,geo", ( "it", "geo", "Georgian (Italy)" ) )
    , ( "it,mac", ( "it", "mac", "Italian (Macintosh)" ) )
    , ( "it,nodeadkeys", ( "it", "nodeadkeys", "Italian (eliminate dead keys)" ) )
    , ( "it,us", ( "it", "us", "Italian (US keyboard with Italian letters)" ) )
    , ( "jp", ( "jp", "", "Japanese" ) )
    , ( "jp,kana", ( "jp", "kana", "Japanese (Kana)" ) )
    , ( "jp,kana86", ( "jp", "kana86", "Japanese (Kana 86)" ) )
    , ( "jp,mac", ( "jp", "mac", "Japanese (Macintosh)" ) )
    , ( "jp,oadg109a", ( "jp", "oadg109a", "Japanese (OADG 109A)" ) )
    , ( "ke", ( "ke", "", "Swahili (Kenya)" ) )
    , ( "ke,kik", ( "ke", "kik", "Kikuyu" ) )
    , ( "kg", ( "kg", "", "Kyrgyz" ) )
    , ( "kg,phonetic", ( "kg", "phonetic", "Kyrgyz (phonetic)" ) )
    , ( "kh", ( "kh", "", "Khmer (Cambodia)" ) )
    , ( "kr", ( "kr", "", "Korean" ) )
    , ( "kr,kr104", ( "kr", "kr104", "Korean (101/104 key compatible)" ) )
    , ( "kz", ( "kz", "", "Kazakh" ) )
    , ( "kz,kazrus", ( "kz", "kazrus", "Kazakh (with Russian)" ) )
    , ( "kz,ruskaz", ( "kz", "ruskaz", "Russian (Kazakhstan, with Kazakh)" ) )
    , ( "la", ( "la", "", "Lao" ) )
    , ( "la,stea", ( "la", "stea", "Lao (STEA proposed standard layout)" ) )
    , ( "latam", ( "latam", "", "Spanish (Latin American)" ) )
    , ( "latam,deadtilde", ( "latam", "deadtilde", "Spanish (Latin American, include dead tilde)" ) )
    , ( "latam,nodeadkeys", ( "latam", "nodeadkeys", "Spanish (Latin American, eliminate dead keys)" ) )
    , ( "latam,sundeadkeys", ( "latam", "sundeadkeys", "Spanish (Latin American, Sun dead keys)" ) )
    , ( "lk", ( "lk", "", "Sinhala (phonetic)" ) )
    , ( "lk,tam_tab", ( "lk", "tam_tab", "Tamil (Sri Lanka, TAB Typewriter)" ) )
    , ( "lk,tam_unicode", ( "lk", "tam_unicode", "Tamil (Sri Lanka, Unicode)" ) )
    , ( "lt", ( "lt", "", "Lithuanian" ) )
    , ( "lt,ibm", ( "lt", "ibm", "Lithuanian (IBM LST 1205-92)" ) )
    , ( "lt,lekp", ( "lt", "lekp", "Lithuanian (LEKP)" ) )
    , ( "lt,lekpa", ( "lt", "lekpa", "Lithuanian (LEKPa)" ) )
    , ( "lt,std", ( "lt", "std", "Lithuanian (standard)" ) )
    , ( "lt,us", ( "lt", "us", "Lithuanian (US keyboard with Lithuanian letters)" ) )
    , ( "lv", ( "lv", "", "Latvian" ) )
    , ( "lv,adapted", ( "lv", "adapted", "Latvian (adapted)" ) )
    , ( "lv,apostrophe", ( "lv", "apostrophe", "Latvian (apostrophe variant)" ) )
    , ( "lv,ergonomic", ( "lv", "ergonomic", "Latvian (ergonomic, ŪGJRMV)" ) )
    , ( "lv,fkey", ( "lv", "fkey", "Latvian (F variant)" ) )
    , ( "lv,modern", ( "lv", "modern", "Latvian (modern)" ) )
    , ( "lv,tilde", ( "lv", "tilde", "Latvian (tilde variant)" ) )
    , ( "ma", ( "ma", "", "Arabic (Morocco)" ) )
    , ( "ma,french", ( "ma", "french", "French (Morocco)" ) )
    , ( "ma,tifinagh", ( "ma", "tifinagh", "Berber (Morocco, Tifinagh)" ) )
    , ( "ma,tifinagh-alt", ( "ma", "tifinagh-alt", "Berber (Morocco, Tifinagh alternative)" ) )
    , ( "ma,tifinagh-alt-phonetic", ( "ma", "tifinagh-alt-phonetic", "Berber (Morocco, Tifinagh alternative phonetic)" ) )
    , ( "ma,tifinagh-extended", ( "ma", "tifinagh-extended", "Berber (Morocco, Tifinagh extended)" ) )
    , ( "ma,tifinagh-extended-phonetic", ( "ma", "tifinagh-extended-phonetic", "Berber (Morocco, Tifinagh extended phonetic)" ) )
    , ( "ma,tifinagh-phonetic", ( "ma", "tifinagh-phonetic", "Berber (Morocco, Tifinagh phonetic)" ) )
    , ( "mao", ( "mao", "", "Maori" ) )
    , ( "me", ( "me", "", "Montenegrin" ) )
    , ( "me,cyrillic", ( "me", "cyrillic", "Montenegrin (Cyrillic)" ) )
    , ( "me,cyrillicalternatequotes", ( "me", "cyrillicalternatequotes", "Montenegrin (Cyrillic with guillemets)" ) )
    , ( "me,cyrillicyz", ( "me", "cyrillicyz", "Montenegrin (Cyrillic, Z and ZHE swapped)" ) )
    , ( "me,latinalternatequotes", ( "me", "latinalternatequotes", "Montenegrin (Latin with guillemets)" ) )
    , ( "me,latinunicode", ( "me", "latinunicode", "Montenegrin (Latin Unicode)" ) )
    , ( "me,latinunicodeyz", ( "me", "latinunicodeyz", "Montenegrin (Latin Unicode qwerty)" ) )
    , ( "me,latinyz", ( "me", "latinyz", "Montenegrin (Latin qwerty)" ) )
    , ( "mk", ( "mk", "", "Macedonian" ) )
    , ( "mk,nodeadkeys", ( "mk", "nodeadkeys", "Macedonian (eliminate dead keys)" ) )
    , ( "ml", ( "ml", "", "Bambara" ) )
    , ( "ml,fr-oss", ( "ml", "fr-oss", "French (Mali, alternative)" ) )
    , ( "ml,us-intl", ( "ml", "us-intl", "English (Mali, US international)" ) )
    , ( "ml,us-mac", ( "ml", "us-mac", "English (Mali, US Macintosh)" ) )
    , ( "mm", ( "mm", "", "Burmese" ) )
    , ( "mn", ( "mn", "", "Mongolian" ) )
    , ( "mt", ( "mt", "", "Maltese" ) )
    , ( "mt,us", ( "mt", "us", "Maltese (with US layout)" ) )
    , ( "mv", ( "mv", "", "Dhivehi" ) )
    , ( "nec_vndr/jp", ( "nec_vndr/jp", "", "Japanese (PC-98xx Series)" ) )
    , ( "ng", ( "ng", "", "English (Nigeria)" ) )
    , ( "ng,hausa", ( "ng", "hausa", "Hausa" ) )
    , ( "ng,igbo", ( "ng", "igbo", "Igbo" ) )
    , ( "ng,yoruba", ( "ng", "yoruba", "Yoruba" ) )
    , ( "nl", ( "nl", "", "Dutch" ) )
    , ( "nl,mac", ( "nl", "mac", "Dutch (Macintosh)" ) )
    , ( "nl,std", ( "nl", "std", "Dutch (standard)" ) )
    , ( "nl,sundeadkeys", ( "nl", "sundeadkeys", "Dutch (Sun dead keys)" ) )
    , ( "no", ( "no", "", "Norwegian" ) )
    , ( "no,dvorak", ( "no", "dvorak", "Norwegian (Dvorak)" ) )
    , ( "no,mac", ( "no", "mac", "Norwegian (Macintosh)" ) )
    , ( "no,mac_nodeadkeys", ( "no", "mac_nodeadkeys", "Norwegian (Macintosh, eliminate dead keys)" ) )
    , ( "no,nodeadkeys", ( "no", "nodeadkeys", "Norwegian (eliminate dead keys)" ) )
    , ( "no,smi", ( "no", "smi", "Northern Saami (Norway)" ) )
    , ( "no,smi_nodeadkeys", ( "no", "smi_nodeadkeys", "Northern Saami (Norway, eliminate dead keys)" ) )
    , ( "np", ( "np", "", "Nepali" ) )
    , ( "ph", ( "ph", "", "Filipino" ) )
    , ( "ph,capewell-dvorak", ( "ph", "capewell-dvorak", "Filipino (Capewell-Dvorak Latin)" ) )
    , ( "ph,capewell-dvorak-bay", ( "ph", "capewell-dvorak-bay", "Filipino (Capewell-Dvorak Baybayin)" ) )
    , ( "ph,capewell-qwerf2k6", ( "ph", "capewell-qwerf2k6", "Filipino (Capewell-QWERF 2006 Latin)" ) )
    , ( "ph,capewell-qwerf2k6-bay", ( "ph", "capewell-qwerf2k6-bay", "Filipino (Capewell-QWERF 2006 Baybayin)" ) )
    , ( "ph,colemak", ( "ph", "colemak", "Filipino (Colemak Latin)" ) )
    , ( "ph,colemak-bay", ( "ph", "colemak-bay", "Filipino (Colemak Baybayin)" ) )
    , ( "ph,dvorak", ( "ph", "dvorak", "Filipino (Dvorak Latin)" ) )
    , ( "ph,dvorak-bay", ( "ph", "dvorak-bay", "Filipino (Dvorak Baybayin)" ) )
    , ( "ph,qwerty-bay", ( "ph", "qwerty-bay", "Filipino (QWERTY Baybayin)" ) )
    , ( "pk", ( "pk", "", "Urdu (Pakistan)" ) )
    , ( "pk,ara", ( "pk", "ara", "Arabic (Pakistan)" ) )
    , ( "pk,snd", ( "pk", "snd", "Sindhi" ) )
    , ( "pk,urd-crulp", ( "pk", "urd-crulp", "Urdu (Pakistan, CRULP)" ) )
    , ( "pk,urd-nla", ( "pk", "urd-nla", "Urdu (Pakistan, NLA)" ) )
    , ( "pl", ( "pl", "", "Polish" ) )
    , ( "pl,csb", ( "pl", "csb", "Kashubian" ) )
    , ( "pl,dvorak", ( "pl", "dvorak", "Polish (Dvorak)" ) )
    , ( "pl,dvorak_altquotes", ( "pl", "dvorak_altquotes", "Polish (Dvorak, Polish quotes on key 1)" ) )
    , ( "pl,dvorak_quotes", ( "pl", "dvorak_quotes", "Polish (Dvorak, Polish quotes on quotemark key)" ) )
    , ( "pl,dvp", ( "pl", "dvp", "Polish (programmer Dvorak)" ) )
    , ( "pl,qwertz", ( "pl", "qwertz", "Polish (qwertz)" ) )
    , ( "pl,ru_phonetic_dvorak", ( "pl", "ru_phonetic_dvorak", "Russian (Poland, phonetic Dvorak)" ) )
    , ( "pt", ( "pt", "", "Portuguese" ) )
    , ( "pt,mac", ( "pt", "mac", "Portuguese (Macintosh)" ) )
    , ( "pt,mac_nodeadkeys", ( "pt", "mac_nodeadkeys", "Portuguese (Macintosh, eliminate dead keys)" ) )
    , ( "pt,mac_sundeadkeys", ( "pt", "mac_sundeadkeys", "Portuguese (Macintosh, Sun dead keys)" ) )
    , ( "pt,nativo", ( "pt", "nativo", "Portuguese (Nativo)" ) )
    , ( "pt,nativo-epo", ( "pt", "nativo-epo", "Esperanto (Portugal, Nativo)" ) )
    , ( "pt,nativo-us", ( "pt", "nativo-us", "Portuguese (Nativo for USA keyboards)" ) )
    , ( "pt,nodeadkeys", ( "pt", "nodeadkeys", "Portuguese (eliminate dead keys)" ) )
    , ( "pt,sundeadkeys", ( "pt", "sundeadkeys", "Portuguese (Sun dead keys)" ) )
    , ( "ro", ( "ro", "", "Romanian" ) )
    , ( "ro,cedilla", ( "ro", "cedilla", "Romanian (cedilla)" ) )
    , ( "ro,std", ( "ro", "std", "Romanian (standard)" ) )
    , ( "ro,std_cedilla", ( "ro", "std_cedilla", "Romanian (standard cedilla)" ) )
    , ( "ro,winkeys", ( "ro", "winkeys", "Romanian (WinKeys)" ) )
    , ( "rs", ( "rs", "", "Serbian (Cyrillic)" ) )
    , ( "rs,alternatequotes", ( "rs", "alternatequotes", "Serbian (Cyrillic with guillemets)" ) )
    , ( "rs,latin", ( "rs", "latin", "Serbian (Latin)" ) )
    , ( "rs,latinalternatequotes", ( "rs", "latinalternatequotes", "Serbian (Latin with guillemets)" ) )
    , ( "rs,latinunicode", ( "rs", "latinunicode", "Serbian (Latin Unicode)" ) )
    , ( "rs,latinunicodeyz", ( "rs", "latinunicodeyz", "Serbian (Latin Unicode qwerty)" ) )
    , ( "rs,latinyz", ( "rs", "latinyz", "Serbian (Latin qwerty)" ) )
    , ( "rs,rue", ( "rs", "rue", "Pannonian Rusyn (homophonic)" ) )
    , ( "rs,yz", ( "rs", "yz", "Serbian (Cyrillic, Z and ZHE swapped)" ) )
    , ( "ru", ( "ru", "", "Russian" ) )
    , ( "ru,bak", ( "ru", "bak", "Bashkirian" ) )
    , ( "ru,chm", ( "ru", "chm", "Mari" ) )
    , ( "ru,cv", ( "ru", "cv", "Chuvash" ) )
    , ( "ru,cv_latin", ( "ru", "cv_latin", "Chuvash (Latin)" ) )
    , ( "ru,dos", ( "ru", "dos", "Russian (DOS)" ) )
    , ( "ru,kom", ( "ru", "kom", "Komi" ) )
    , ( "ru,legacy", ( "ru", "legacy", "Russian (legacy)" ) )
    , ( "ru,os_legacy", ( "ru", "os_legacy", "Ossetian (legacy)" ) )
    , ( "ru,os_winkeys", ( "ru", "os_winkeys", "Ossetian (WinKeys)" ) )
    , ( "ru,phonetic", ( "ru", "phonetic", "Russian (phonetic)" ) )
    , ( "ru,phonetic_winkeys", ( "ru", "phonetic_winkeys", "Russian (phonetic WinKeys)" ) )
    , ( "ru,sah", ( "ru", "sah", "Yakut" ) )
    , ( "ru,srp", ( "ru", "srp", "Serbian (Russia)" ) )
    , ( "ru,tt", ( "ru", "tt", "Tatar" ) )
    , ( "ru,typewriter", ( "ru", "typewriter", "Russian (typewriter)" ) )
    , ( "ru,typewriter-legacy", ( "ru", "typewriter-legacy", "Russian (typewriter, legacy)" ) )
    , ( "ru,udm", ( "ru", "udm", "Udmurt" ) )
    , ( "ru,xal", ( "ru", "xal", "Kalmyk" ) )
    , ( "se", ( "se", "", "Swedish" ) )
    , ( "se,dvorak", ( "se", "dvorak", "Swedish (Dvorak)" ) )
    , ( "se,mac", ( "se", "mac", "Swedish (Macintosh)" ) )
    , ( "se,nodeadkeys", ( "se", "nodeadkeys", "Swedish (eliminate dead keys)" ) )
    , ( "se,rus", ( "se", "rus", "Russian (Sweden, phonetic)" ) )
    , ( "se,rus_nodeadkeys", ( "se", "rus_nodeadkeys", "Russian (Sweden, phonetic, eliminate dead keys)" ) )
    , ( "se,smi", ( "se", "smi", "Northern Saami (Sweden)" ) )
    , ( "se,svdvorak", ( "se", "svdvorak", "Swedish (Svdvorak)" ) )
    , ( "se,swl", ( "se", "swl", "Swedish Sign Language" ) )
    , ( "si", ( "si", "", "Slovenian" ) )
    , ( "si,alternatequotes", ( "si", "alternatequotes", "Slovenian (use guillemets for quotes)" ) )
    , ( "si,us", ( "si", "us", "Slovenian (US keyboard with Slovenian letters)" ) )
    , ( "sk", ( "sk", "", "Slovak" ) )
    , ( "sk,bksl", ( "sk", "bksl", "Slovak (extended Backslash)" ) )
    , ( "sk,qwerty", ( "sk", "qwerty", "Slovak (qwerty)" ) )
    , ( "sk,qwerty_bksl", ( "sk", "qwerty_bksl", "Slovak (qwerty, extended Backslash)" ) )
    , ( "sn", ( "sn", "", "Wolof" ) )
    , ( "sy", ( "sy", "", "Arabic (Syria)" ) )
    , ( "sy,ku", ( "sy", "ku", "Kurdish (Syria, Latin Q)" ) )
    , ( "sy,ku_alt", ( "sy", "ku_alt", "Kurdish (Syria, Latin Alt-Q)" ) )
    , ( "sy,ku_f", ( "sy", "ku_f", "Kurdish (Syria, F)" ) )
    , ( "sy,syc", ( "sy", "syc", "Syriac" ) )
    , ( "sy,syc_phonetic", ( "sy", "syc_phonetic", "Syriac (phonetic)" ) )
    , ( "th", ( "th", "", "Thai" ) )
    , ( "th,pat", ( "th", "pat", "Thai (Pattachote)" ) )
    , ( "th,tis", ( "th", "tis", "Thai (TIS-820.2538)" ) )
    , ( "tj", ( "tj", "", "Tajik" ) )
    , ( "tj,legacy", ( "tj", "legacy", "Tajik (legacy)" ) )
    , ( "tm", ( "tm", "", "Turkmen" ) )
    , ( "tm,alt", ( "tm", "alt", "Turkmen (Alt-Q)" ) )
    , ( "tr", ( "tr", "", "Turkish" ) )
    , ( "tr,alt", ( "tr", "alt", "Turkish (Alt-Q)" ) )
    , ( "tr,crh", ( "tr", "crh", "Crimean Tatar (Turkish Q)" ) )
    , ( "tr,crh_alt", ( "tr", "crh_alt", "Crimean Tatar (Turkish Alt-Q)" ) )
    , ( "tr,crh_f", ( "tr", "crh_f", "Crimean Tatar (Turkish F)" ) )
    , ( "tr,f", ( "tr", "f", "Turkish (F)" ) )
    , ( "tr,intl", ( "tr", "intl", "Turkish (international with dead keys)" ) )
    , ( "tr,ku", ( "tr", "ku", "Kurdish (Turkey, Latin Q)" ) )
    , ( "tr,ku_alt", ( "tr", "ku_alt", "Kurdish (Turkey, Latin Alt-Q)" ) )
    , ( "tr,ku_f", ( "tr", "ku_f", "Kurdish (Turkey, F)" ) )
    , ( "tr,sundeadkeys", ( "tr", "sundeadkeys", "Turkish (Sun dead keys)" ) )
    , ( "tw", ( "tw", "", "Taiwanese" ) )
    , ( "tw,indigenous", ( "tw", "indigenous", "Taiwanese (indigenous)" ) )
    , ( "tw,saisiyat", ( "tw", "saisiyat", "Saisiyat (Taiwan)" ) )
    , ( "tz", ( "tz", "", "Swahili (Tanzania)" ) )
    , ( "ua", ( "ua", "", "Ukrainian" ) )
    , ( "ua,homophonic", ( "ua", "homophonic", "Ukrainian (homophonic)" ) )
    , ( "ua,legacy", ( "ua", "legacy", "Ukrainian (legacy)" ) )
    , ( "ua,phonetic", ( "ua", "phonetic", "Ukrainian (phonetic)" ) )
    , ( "ua,rstu", ( "ua", "rstu", "Ukrainian (standard RSTU)" ) )
    , ( "ua,rstu_ru", ( "ua", "rstu_ru", "Russian (Ukraine, standard RSTU)" ) )
    , ( "ua,typewriter", ( "ua", "typewriter", "Ukrainian (typewriter)" ) )
    , ( "ua,winkeys", ( "ua", "winkeys", "Ukrainian (WinKeys)" ) )
    , ( "us", ( "us", "", "English (US)" ) )
    , ( "us,alt-intl", ( "us", "alt-intl", "English (US, alternative international)" ) )
    , ( "us,altgr-intl", ( "us", "altgr-intl", "English (international AltGr dead keys)" ) )
    , ( "us,chr", ( "us", "chr", "Cherokee" ) )
    , ( "us,colemak", ( "us", "colemak", "English (Colemak)" ) )
    , ( "us,dvorak", ( "us", "dvorak", "English (Dvorak)" ) )
    , ( "us,dvorak-alt-intl", ( "us", "dvorak-alt-intl", "English (Dvorak alternative international no dead keys)" ) )
    , ( "us,dvorak-classic", ( "us", "dvorak-classic", "English (classic Dvorak)" ) )
    , ( "us,dvorak-intl", ( "us", "dvorak-intl", "English (Dvorak international with dead keys)" ) )
    , ( "us,dvorak-l", ( "us", "dvorak-l", "English (left handed Dvorak)" ) )
    , ( "us,dvorak-r", ( "us", "dvorak-r", "English (right handed Dvorak)" ) )
    , ( "us,dvp", ( "us", "dvp", "English (programmer Dvorak)" ) )
    , ( "us,euro", ( "us", "euro", "English (US, with euro on 5)" ) )
    , ( "us,hbs", ( "us", "hbs", "Serbo-Croatian (US)" ) )
    , ( "us,intl", ( "us", "intl", "English (US, international with dead keys)" ) )
    , ( "us,mac", ( "us", "mac", "English (Macintosh)" ) )
    , ( "us,olpc2", ( "us", "olpc2", "English (layout toggle on multiply/divide key)" ) )
    , ( "us,rus", ( "us", "rus", "Russian (US, phonetic)" ) )
    , ( "uz", ( "uz", "", "Uzbek" ) )
    , ( "uz,latin", ( "uz", "latin", "Uzbek (Latin)" ) )
    , ( "vn", ( "vn", "", "Vietnamese" ) )
    , ( "za", ( "za", "", "English (South Africa)" ) )
    ]