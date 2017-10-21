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
    , draftedPlayers : List ( Player, String )
    , waitingTeams : List Team
    , draftedTeams : List Team
    , round : Int
    , currentView : TabView
    , history : History
    }


type History
    = History (List Model)


type TabView
    = DraftView
    | TeamView
    | HistoryView


initModel : Model
initModel =
    { undraftedPlayers = Players.players
    , draftedPlayers = []
    , waitingTeams = Teams.teams
    , draftedTeams = []
    , round = 1
    , currentView = DraftView
    , history = History ([])
    }



-- UPDATE


type Msg
    = Draft Player
    | FlipOrder
    | UndoRound
    | Reset
    | ChangeView TabView


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

        ChangeView tabView ->
            { model | currentView = tabView }


draftPlayer : Player -> Model -> Model
draftPlayer player model =
    let
        remaining =
            List.filter (\p -> p /= player) model.undraftedPlayers

        draftingTeam =
            List.head model.waitingTeams
                |> addPlayer player

        ( newWaiting, newDrafted ) =
            updateRound draftingTeam model

        draftingTeamName =
            case draftingTeam of
                Just team ->
                    team.gm ++ ":"

                Nothing ->
                    "Unknown Team"

        ( round, history ) =
            if List.isEmpty newDrafted then
                ( model.round + 1, updateHistory model )
            else
                ( model.round, model.history )
    in
        { model
            | undraftedPlayers = remaining
            , draftedPlayers = ( player, draftingTeamName ) :: model.draftedPlayers
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
        :: viewTabNav model.currentView
        :: viewTabContent model
        |> div []


styles : Html Msg
styles =
    Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []


title : Html Msg
title =
    h1 []
        [ text "Parity Draft"
        , div []
            [ button [ onClick FlipOrder ] [ text "Invert Order" ]
            , button [ onClick UndoRound ] [ text "Undo Round" ]
            ]
        ]


viewTabNav : TabView -> Html Msg
viewTabNav currentView =
    let
        buildTab tabView title =
            let
                className =
                    if tabView == currentView then
                        "active"
                    else
                        ""
            in
                div
                    [ class className
                    , onClick (ChangeView tabView)
                    ]
                    [ text title ]
    in
        div [ class "nav" ]
            [ buildTab DraftView "Draft"
            , buildTab TeamView "Teams"
            , buildTab HistoryView "History"
            ]


viewTabContent : Model -> List (Html Msg)
viewTabContent model =
    case model.currentView of
        DraftView ->
            viewDraftContent model

        TeamView ->
            viewDraftComplete model

        HistoryView ->
            viewDraftHistory model


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
    , viewPlayerList "Players" draftablePlayer model.undraftedPlayers

    --, viewTeamsLastDrafted (viewRound model) model.draftedTeams
    , viewTeamsWithLatest model.round model.draftedTeams
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
    [ viewPlayerList "Draft History" viewDraftedPlayer model.draftedPlayers
    , viewPlayerList "Draft Order" viewDraftedPlayer (List.reverse model.draftedPlayers)
    ]


viewTeamsWithLatest : Int -> List Team -> Html Msg
viewTeamsWithLatest round teams =
    let
        formatPlayer player =
            let
                className =
                    Players.className player
            in
                dd [ class className ] [ text player.name ]

        title =
            "Round " ++ (toString round)

        viewPlayerList team =
            case List.head team.players of
                Just player ->
                    dt [] [ text team.gm, formatPlayer player ]

                Nothing ->
                    text title

        teamList =
            List.map viewPlayerList teams
    in
        segment title "" [ dl [] teamList ]


viewTeamWithRoster : Bool -> Team -> Html Msg
viewTeamWithRoster format team =
    let
        viewPlayerList =
            List.reverse team.players
                |> List.map (viewPlayerDetail [] False)

        ( start, end ) =
            if format then
                ( h3 [] [ text team.gm ], [ br [] [], br [] [] ] )
            else
                ( text "", [] )
    in
        start
            :: [ ul [ class "players" ] viewPlayerList ]
            ++ end
            |> div [ class "team" ]


viewPlayerDetail : List (Attribute Msg) -> Bool -> Player -> Html Msg
viewPlayerDetail attributes details player =
    let
        className =
            Players.className player

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


viewPlayerList : String -> (a -> Html Msg) -> List a -> Html Msg
viewPlayerList title view list =
    segment title "" [ ol [ class "players" ] (List.map view list) ]


viewDraftedPlayer : ( Player, String ) -> Html Msg
viewDraftedPlayer playerInfo =
    let
        ( player, gm ) =
            playerInfo

        className =
            Players.className player
    in
        li []
            [ div [ class "gm" ] [ text gm ]
            , div [ class className ] [ text player.name ]
            ]


draftablePlayer : Player -> Html Msg
draftablePlayer player =
    viewPlayerDetail [ class "draftable", onClick (Draft player) ] True player


segment : String -> String -> List (Html Msg) -> Html Msg
segment title className list =
    div [ class "segment", class className ]
        (h2 [] [ text title ] :: list)
