module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Data exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { teams : Array Team
    , undrafted : List Player
    , drafted : List Player
    , currentTeamIndex : Int
    }


initModel : Model
initModel =
    { teams = Array.fromList Data.teams
    , undrafted = Data.players
    , drafted = []
    , currentTeamIndex = 0
    }


currentTeam : Model -> Maybe Team
currentTeam model =
    Array.get model.currentTeamIndex model.teams



-- UPDATE


type Msg
    = Draft Player
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Draft player ->
            draftPlayer player model

        Reset ->
            initModel


draftPlayer : Player -> Model -> Model
draftPlayer player model =
    let
        remaining =
            List.filter (\p -> p /= player) model.undrafted

        teams =
            addPlayer player model
    in
        { model
            | undrafted = remaining
            , drafted = player :: model.drafted
            , currentTeamIndex = updateDraftOrder model
            , teams = teams
        }


updateDraftOrder : Model -> Int
updateDraftOrder model =
    if model.currentTeamIndex + 1 >= (Array.length model.teams) then
        0
    else
        model.currentTeamIndex + 1


addPlayer : Player -> Model -> Array Team
addPlayer player model =
    case currentTeam model of
        Just team ->
            let
                updated =
                    { team | players = player :: team.players }
            in
                Array.set model.currentTeamIndex updated model.teams

        Nothing ->
            model.teams



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Mock Draft" ]
        , div []
            [ button [ onClick Reset ] [ text "Reset" ]
            ]
        , viewCurrentTeam model
        , displayTeams (Array.toList model.teams)
        , playerList "Player List" True model.undrafted
        , playerList "Draft History" False model.drafted
        , playerList "Draft Order" False (List.reverse model.drafted)
        ]


viewCurrentTeam : Model -> Html Msg
viewCurrentTeam model =
    case currentTeam model of
        Just team ->
            viewTeam team

        Nothing ->
            h3 [] [ text "Unknown Team!" ]


viewTeam : Team -> Html Msg
viewTeam team =
    let
        playerList =
            team.gm :: team.players
                |> viewRoster
    in
        div []
            <| [ h3 [] [ text team.name ] ]
            ++ playerList


viewRoster : List Player -> List (Html Msg)
viewRoster players =
    List.map viewPlayer players


viewPlayer : Player -> Html Msg
viewPlayer player =
    div [] [ text player ]


displayTeams : List Team -> Html Msg
displayTeams teams =
    div segment
        ([ h2 [] [ text "Teams" ]
         ]
            ++ (List.map teamItem teams)
        )


teamItem : Team -> Html Msg
teamItem team =
    li [] [ text team.name ]


playerList : String -> Bool -> List Player -> Html Msg
playerList title active players =
    div segment
        [ h2 [] [ text title ]
        , displayPlayers active players
        ]


displayPlayers : Bool -> List Player -> Html Msg
displayPlayers draftable players =
    ol [] (List.map (\p -> draftablePlayer p draftable) players)


draftablePlayer : Player -> Bool -> Html Msg
draftablePlayer player draftable =
    let
        action =
            if draftable then
                [ onClick (Draft player) ]
            else
                []
    in
        li action [ text player ]


segment : List (Attribute msg)
segment =
    [ style
        [ ( "width", "200px" )
        , ( "display", "inline-block" )
        , ( "vertical-align", "top" )
        , ( "background-color", "lightgreen" )
        , ( "border", "solid 2px green" )
        , ( "margin", "1em" )
        , ( "padding", "1em" )
        ]
    , class "segment"
    ]
