port module Main exposing (..)

import Html
import Teams exposing (..)
import Players exposing (..)
import Update exposing (Msg, update)
import Model exposing (..)
import View
import Format
import Navigation exposing (Location)


main : Program (Maybe Model) Model Msg
main =
    Navigation.programWithFlags Update.OnLocationChange
        { init = init
        , view = View.view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model

        cleanModel =
            { newModel | showMenu = False }
    in
        ( newModel
        , Cmd.batch [ saveModel cleanModel, cmds ]
        )


init : Maybe Model -> Location -> ( Model, Cmd Msg )
init savedModel location =
    let
        ( hostType, hostId ) =
            Model.parseLocation location

        newModel =
            case savedModel of
                Just m ->
                    { m
                        | hostingType = hostType
                        , hostingId = hostId
                    }

                Nothing ->
                    initModel hostType hostId
    in
        newModel ! []



-- PORTS


port saveModel : Model -> Cmd msg
