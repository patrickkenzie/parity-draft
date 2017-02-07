module Main exposing (..)

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
    { undraftedPlayers : List Player
    , draftedPlayers : List Player
    , waitingTeams : List Team
    , draftedTeams : List Team
    , round : Int
    }


initModel : Model
initModel =
    { undraftedPlayers = Data.players
    , draftedPlayers = []
    , waitingTeams = Data.teams
    , draftedTeams = []
    , round = 1
    }


currentTeam : Model -> Maybe Team
currentTeam model =
    List.head model.waitingTeams



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
            List.filter (\p -> p /= player) model.undraftedPlayers

        draftingTeam =
            currentTeam model
                |> addPlayer player

        ( newWaiting, newDrafted ) =
            updateRound draftingTeam model

        round =
            if List.isEmpty newDrafted then
                model.round + 1
            else
                model.round
    in
        { model
            | undraftedPlayers = remaining
            , draftedPlayers = player :: model.draftedPlayers
            , waitingTeams = newWaiting
            , draftedTeams = newDrafted
            , round = round
        }


updateRound : Maybe Team -> Model -> ( List Team, List Team )
updateRound team model =
    let
        drafted =
            case team of
                Just team ->
                    team :: model.draftedTeams

                Nothing ->
                    model.draftedTeams

        waiting =
            case List.tail model.waitingTeams of
                Just rest ->
                    rest

                Nothing ->
                    []
    in
        if List.isEmpty waiting then
            ( drafted, waiting )
        else
            ( waiting, drafted )


addPlayer : Player -> Maybe Team -> Maybe Team
addPlayer player team =
    case team of
        Just team ->
            Just { team | players = team.players ++ [ player ] }

        Nothing ->
            team



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Mock Draft" ]
        , viewCurrentTeam model
        , viewTeamNames "Up Next" model.waitingTeams
        , playerList "Players" draftablePlayer model.undraftedPlayers
        , viewTeamLastDrafted (viewRound model) model.draftedTeams
        , playerList "Draft History" viewPlayer model.draftedPlayers
        , playerList "Draft Order" viewPlayer (List.reverse model.draftedPlayers)
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        ]


viewRound : Model -> String
viewRound model =
    "Round " ++ toString model.round


viewCurrentTeam : Model -> Html Msg
viewCurrentTeam model =
    case currentTeam model of
        Just team ->
            viewTeamWithRoster team

        Nothing ->
            h3 [] [ text "Unknown Team!" ]


viewTeam : List (Html Msg) -> Team -> Html Msg
viewTeam playerList team =
    div [ class "team" ]
        [ h3 [] [ text team.name ]
        , ul [ class "players" ] playerList
        ]


viewTeamWithLatest : Team -> Html Msg
viewTeamWithLatest team =
    let
        player =
            case List.head team.players of
                Just player ->
                    player

                Nothing ->
                    team.gm

        playerList =
            [ viewPlayer player ]
    in
        viewTeam playerList team


viewTeamWithRoster : Team -> Html Msg
viewTeamWithRoster team =
    let
        playerList =
            List.map viewPlayer (team.gm :: team.players)
    in
        viewTeam playerList team


viewPlayer : Player -> Html Msg
viewPlayer player =
    li [] [ text player ]


viewTeamNames : String -> List Team -> Html Msg
viewTeamNames title teams =
    viewTeamList title teams (viewTeam [])


viewTeamLastDrafted : String -> List Team -> Html Msg
viewTeamLastDrafted title teams =
    viewTeamList title teams viewTeamWithLatest


viewTeamList : String -> List Team -> (Team -> Html Msg) -> Html Msg
viewTeamList title teams view =
    div segment <|
        [ h2 [] [ text title ] ]
            ++ (List.map view teams)


playerList : String -> (Player -> Html Msg) -> List Player -> Html Msg
playerList title view players =
    div segment
        [ h2 [] [ text title ]
        , ol [ class "playerList" ] (List.map view players)
        ]


draftablePlayer : Player -> Html Msg
draftablePlayer player =
    li [] [ button [ onClick (Draft player) ] [ text player ] ]


segment : List (Attribute msg)
segment =
    [ class "segment" ]


playerListAttributes : List (Attribute msg)
playerListAttributes =
    [ class "playerList" ]
