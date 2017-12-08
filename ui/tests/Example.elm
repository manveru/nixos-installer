module Example exposing (..)

import Expect exposing (Expectation, equal)
import NixosTypeParser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The NixosTypeParser"
        [ describe "TypeParser.parse"
            [ test
                "parses all the types"
              <|
                \_ ->
                    Expect.equal (Ok NixString) (parse "string")
            , test "strings" <|
                \_ ->
                    Expect.equal (Ok NixString) (parse "string")
            , test "boolean" <|
                \_ ->
                    Expect.equal (Ok NixBoolean) (parse "boolean")
            , test "integer" <|
                \_ ->
                    Expect.equal (Ok NixInteger) (parse "integer")
            , test "null or integer" <|
                \_ ->
                    Expect.equal (Ok NixInteger) (parse "null or integer")
            ]
        ]
