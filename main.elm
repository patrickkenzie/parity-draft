port module Main exposing (..)

import Html
import Teams exposing (..)
import Players exposing (..)
import Update exposing(Msg, update)
import Model exposing (..)
import View
import Format


main : Program (Maybe Model) Model Msg
main =
    Html.programWithFlags
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
    in
        ( newModel
        , Cmd.batch [ saveModel newModel, cmds ]
        )


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    Maybe.withDefault initModel savedModel ! []



-- PORTS


port saveModel : Model -> Cmd msg
