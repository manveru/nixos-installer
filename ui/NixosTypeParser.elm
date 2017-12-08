module NixosTypeParser exposing (NixType(..), parse)

import Parser exposing ((|.), (|=), Parser, float, ignore, keyword, succeed, symbol, zeroOrMore)


type NixType
    = NixString
    | NixBoolean
    | NixInteger


parse : String -> Result Parser.Error NixType
parse input =
    Parser.run nixType input


nixType : Parser NixType
nixType =
    Parser.oneOf
        [ succeed NixString
            |. keyword "string"
        , succeed
            NixBoolean
            |. keyword "boolean"
        , succeed
            NixInteger
            |. keyword "integer"
        , succeed
            NixInteger
            |. keyword "null or integer"
        , succeed
            NixInteger
            |. keyword "null or string"
        ]
