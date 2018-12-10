port module Main exposing (..)

import Html
import Teams exposing (..)
import Players exposing (..)
import Update exposing (Msg, update, loadModel)
import Model exposing (..)
import View
import Format
import Navigation exposing (Location)
import Json.Encode as J exposing (..)
import Json.Decode as D exposing (..)
import Time exposing (every, second)


main : Program J.Value Model Msg
main =
    Navigation.programWithFlags (Update.LocalMsg << Update.OnLocationChange)
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
    in
        ( newModel
        , Cmd.batch [ saveModel (encodeModel newModel), cmds ]
        )


init : J.Value -> Location -> ( Model, Cmd Msg )
init savedModel location =
    let
        hostType =
            Model.parseLocation location

        localState =
            { currentView = DraftView
            , playerSearch = ""
            , playerSorts = []
            , showMenu = False
            , hostingType = hostType
            }

        command =
            case hostType of
                View _ ->
                    Update.loadModel localState

                Host _ ->
                    Cmd.none

                Local ->
                    Cmd.none
    in
        case decodeModel savedModel localState of
            Just model ->
                ( model, command )

            Nothing ->
                ( initModel localState, command )



-- PORTS


port saveModel : J.Value -> Cmd msg



-- SUBSCRIPTIONS


subs : Model -> Sub Msg
subs model =
    case model.localState.hostingType of
        View _ ->
            Time.every (10 * 1000) (always Update.RequestModelUpdate)

        _ ->
            Sub.none
