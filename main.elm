module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Teams exposing (..)
import Players exposing (..)
import Format


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
    , history : History
    }


type History
    = History (List Model)


initModel : Model
initModel =
    { undraftedPlayers = Players.players
    , draftedPlayers = []
    , waitingTeams = Teams.teams
    , draftedTeams = []
    , round = 1
    , history = History ([])
    }


currentTeam : Model -> Maybe Team
currentTeam model =
    List.head model.waitingTeams



-- UPDATE


type Msg
    = Draft Player
    | FlipOrder
    | UndoRound
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Draft player ->
            draftPlayer player model

        FlipOrder ->
            { model | waitingTeams = List.reverse model.waitingTeams }

        UndoRound ->
            popHistory model.history

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

        ( round, history ) =
            if List.isEmpty newDrafted then
                ( model.round + 1, updateHistory model )
            else
                ( model.round, model.history )
    in
        { model
            | undraftedPlayers = remaining
            , draftedPlayers = player :: model.draftedPlayers
            , waitingTeams = newWaiting
            , draftedTeams = newDrafted
            , round = round
            , history = history
        }


updateHistory : Model -> History
updateHistory model =
    let
        (History existing) =
            model.history
    in
        History (model :: existing)


popHistory : History -> Model
popHistory history =
    let
        (History rounds) =
            history
    in
        if List.isEmpty rounds then
            initModel
        else
            case List.head rounds of
                Just round ->
                    round

                Nothing ->
                    initModel


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
            Just { team | players = player :: team.players }

        Nothing ->
            team



-- VIEW


view : Model -> Html Msg
view model =
    styles
        :: title
        :: viewDraftContent model
        |> div []


styles : Html Msg
styles =
    Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []


title : Html Msg
title =
    h1 []
        [ text "Mock Draft"
        , div []
            [ button [ onClick FlipOrder ] [ text "Invert Order" ]
            , button [ onClick UndoRound ] [ text "Undo Round" ]
            ]
        ]


viewDraftContent : Model -> List (Html Msg)
viewDraftContent model =
    if List.isEmpty model.undraftedPlayers then
        viewDraftComplete model
    else
        viewDraftInProgress model
            ++ viewDraftHistory model


viewDraftInProgress : Model -> List (Html Msg)
viewDraftInProgress model =
    [ viewWaitingTeams model.waitingTeams
    , playerList "Players" draftablePlayer model.undraftedPlayers
      --, viewTeamsLastDrafted (viewRound model) model.draftedTeams
    , viewTeamsWithLatest (viewRound model) model.draftedTeams
    ]


viewDraftComplete : Model -> List (Html Msg)
viewDraftComplete model =
    let
        teams =
            model.draftedTeams ++ model.waitingTeams

        teamDisplay =
            List.map (viewTeamWithRoster True) teams
    in
        [ div [ id "draftResults" ] teamDisplay ]


viewDraftHistory : Model -> List (Html Msg)
viewDraftHistory model =
    [ playerList "Draft History" viewPlayer model.draftedPlayers
    , playerList "Draft Order" viewPlayer (List.reverse model.draftedPlayers)
    ]


viewRound : Model -> String
viewRound model =
    "Round " ++ toString model.round


viewTeamsWithLatest : String -> List Team -> Html Msg
viewTeamsWithLatest title teams =
    let
        formatPlayer player =
            let
                className =
                    if player.gender == Female then
                        "female"
                    else
                        "male"
            in
                dd [ class className ] [ text player.name ]

        playerList team =
            case List.head team.players of
                Just player ->
                    dt [] [ text team.gm, formatPlayer player ]

                Nothing ->
                    text title

        teamList =
            List.map playerList teams
    in
        segment title "" [ dl [] teamList ]


viewTeamWithRoster : Bool -> Team -> Html Msg
viewTeamWithRoster format team =
    let
        playerList =
            List.reverse team.players
                |> List.map viewPlayer

        ( start, end ) =
            if format then
                ( h3 [] [ text team.gm ], [ br [] [], br [] [] ] )
            else
                ( text "", [] )
    in
        start
            :: [ ul [ class "players" ] playerList ]
            ++ end
            |> div [ class "team" ]


viewPlayer : Player -> Html Msg
viewPlayer player =
    viewPlayerDetail [] False player


viewPlayerDetail : List (Attribute Msg) -> Bool -> Player -> Html Msg
viewPlayerDetail attributes details player =
    let
        className =
            if player.gender == Female then
                "female"
            else
                "male"

        rating =
            span [ class "rating" ] [ text (toString player.rating) ]

        salary =
            rating
                :: [ text (Format.formatSalary player.salary) ]
                |> span [ class "salary" ]

        content =
            if details then
                [ text player.name
                , salary
                ]
            else
                [ text player.name ]
    in
        li ([ class className ] ++ attributes) content


viewWaitingTeams : List Team -> Html Msg
viewWaitingTeams teams =
    let
        ( currentTeam, title ) =
            case List.head teams of
                Just team ->
                    ( viewTeamWithRoster False team, team.gm )

                Nothing ->
                    ( text "Unknown Team!", "Unknown!" )

        teamList =
            case List.tail teams of
                Just list ->
                    List.map (\t -> li [] [ text t.gm ]) list
                        |> ul [ class "teams" ]

                Nothing ->
                    ul [] []

        upNext =
            [ h3 [] [ text "Up Next" ], teamList ]
    in
        currentTeam
            :: upNext
            |> segment title ""


playerList : String -> (Player -> Html Msg) -> List Player -> Html Msg
playerList title view players =
    segment title "" [ ol [ class "players" ] (List.map view players) ]


draftablePlayer : Player -> Html Msg
draftablePlayer player =
    viewPlayerDetail [ class "draftable", onClick (Draft player) ] True player


segment : String -> String -> List (Html Msg) -> Html Msg
segment title className list =
    div [ class "segment", class className ]
        (h2 [] [ text title ] :: list)


playerListAttributes : List (Attribute msg)
playerListAttributes =
    [ class "players" ]
