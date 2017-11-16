module UserTab.State exposing (init, subscriptions, update)

import App.Types exposing (..)
import Material
import SHA512Crypt


subscriptions : Model -> Sub Msg
subscriptions model =
    SHA512Crypt.hash (\hash -> SetPassHash hash)


update :
    Msg
    -> { b | user : Model, mdl : Material.Model }
    -> ( Material.Container { b | user : Model }, Cmd Msg )
update message model =
    let
        userModel =
            model.user

        userModelUser =
            userModel.user

        userModelUsers =
            userModel.users
    in
    case message of
        AddUser new ->
            ( { model | user = { userModel | users = userModelUsers ++ [ new ] } }, Cmd.none )

        NewUser ->
            ( { model | user = { userModel | user = nullUser } }, Cmd.none )

        SetName name ->
            ( { model | user = { userModel | user = { userModelUser | name = name } } }, Cmd.none )

        SetFullName fullName ->
            ( { model | user = { userModel | user = { userModelUser | fullName = fullName } } }, Cmd.none )

        SetPassHash hash ->
            ( { model
                | user =
                    { userModel
                        | user =
                            { userModelUser
                                | passHash = hash
                            }
                    }
              }
            , Cmd.none
            )

        SetPass pass ->
            ( { model
                | user =
                    { userModel
                        | user =
                            { userModelUser
                                | pass =
                                    pass
                            }
                    }
              }
            , crypt pass
            )

        SetPassAgain passAgain ->
            ( { model | user = { userModel | user = { userModelUser | passAgain = passAgain } } }, Cmd.none )

        Mdl m ->
            Material.update Mdl m model


salt : String
salt =
    "Gah4maig2It4ooraiChee5xeequ6quiogih3dei9mak2Om7hahzungohC4Eich8h"


crypt : String -> Cmd msg
crypt password =
    SHA512Crypt.crypt ( password, salt )
