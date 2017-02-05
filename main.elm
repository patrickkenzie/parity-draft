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
    }


initModel : Model
initModel =
    { undraftedPlayers = Data.players
    , draftedPlayers = []
    , waitingTeams = Data.teams
    , draftedTeams = []
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
            updateTeams draftingTeam model
    in
        { model
            | undraftedPlayers = remaining
            , draftedPlayers = player :: model.draftedPlayers
            , waitingTeams = newWaiting
            , draftedTeams = newDrafted
        }


updateTeams : Maybe Team -> Model -> ( List Team, List Team )
updateTeams team model =
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
        , div []
            [ button [ onClick Reset ] [ text "Reset" ]
            ]
        , viewCurrentTeam model
        , displayTeams "Up Next" model.waitingTeams
        , displayTeams "Previous" model.draftedTeams
        , playerList "Player List" True model.undraftedPlayers
        , playerList "Draft History" False model.draftedPlayers
        , playerList "Draft Order" False (List.reverse model.draftedPlayers)
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
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
            List.map viewPlayer team.players
    in
        div [] <|
            [ h3 [] [ text team.name ] ]
                ++ [ viewPlayer team.gm ]
                ++ playerList


viewPlayer : Player -> Html Msg
viewPlayer player =
    div [] [ text player ]


displayTeams : String -> List Team -> Html Msg
displayTeams title teams =
    div segment <|
        [ h2 [] [ text title ] ]
            ++ (List.map viewTeam teams)


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
    [ class "segment"]
