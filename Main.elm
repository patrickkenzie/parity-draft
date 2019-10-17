port module Main exposing (init, main, saveModel, subs, updateWithStorage)

import Browser
import Browser.Navigation
import Format
import Html
import Json.Decode as D exposing (..)
import Json.Encode as J exposing (..)
import Model exposing (..)
import Players exposing (..)
import Teams exposing (..)
import Time exposing (every)
import Update exposing (Msg, loadModel, update)
import Url
import View


main : Program J.Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = Update.LocalMsg << Update.OnLocationChange
        , onUrlRequest = \_ -> Update.NoOp
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


init : J.Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init savedModel location key =
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
            Time.every (10 * 1000) (always (Update.LocalMsg Update.RequestModelUpdate))

        _ ->
            Sub.none
