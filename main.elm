port module Main exposing (..)

import Html
import Teams exposing (..)
import Players exposing (..)
import Update exposing (Msg, update)
import Model exposing (..)
import View
import Format
import Navigation exposing (Location)
import Json.Encode as J exposing (..)
import Json.Decode as D exposing (..)
import Time exposing (every, second)


main : Program J.Value Model Msg
main =
    Navigation.programWithFlags Update.OnLocationChange
        { init = init
        , view = View.view
        , update = updateWithStorage
        , subscriptions = subs
        }


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model

        cleanModel =
            encodeModel { newModel | showMenu = False }
    in
        ( newModel
        , Cmd.batch [ saveModel cleanModel, cmds ]
        )


init : J.Value -> Location -> ( Model, Cmd Msg )
init savedModel location =
    let
        ( hostType, hostId ) =
            Model.parseLocation location

        newModel =
            case decodeModel savedModel hostType hostId of
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


port saveModel : J.Value -> Cmd msg



-- SUBSCRIPTIONS


subs : Model -> Sub Msg
subs model =
    if model.hostingType == "view" then
        Time.every (10 * 1000) (always Update.RequestModelUpdate)
    else
        Sub.none
