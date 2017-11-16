port module SHA512Crypt exposing (crypt, hash)


port crypt : ( String, String ) -> Cmd msg


port hash : (String -> msg) -> Sub msg
