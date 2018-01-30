port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Teams exposing (..)
import Players exposing (..)
import Format


main : Program (Maybe Model) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
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



-- MODEL


type alias Model =
    { undraftedPlayers : List Player
    , draftedPlayers : List ( Player, String )
    , waitingTeams : List Team
    , draftedTeams : List Team
    , round : Int
    , currentView : Int
    }


type TabView
    = DraftView
    | TeamView
    | HistoryView


type alias PlayerSort =
    Player -> Player -> Order


compareByAsc : (Player -> comparable) -> Player -> Player -> Order
compareByAsc sort x y =
    compare (sort x) (sort y)


compareByDesc : (Player -> comparable) -> Player -> Player -> Order
compareByDesc sort x y =
    compare (sort y) (sort x)


initModel : Model
initModel =
    { undraftedPlayers = Players.players
    , draftedPlayers = []
    , waitingTeams = Teams.teams
    , draftedTeams = []
    , round = 1
    , currentView = 0
    }


tabViewFromInt : Int -> TabView
tabViewFromInt value =
    case value of
        1 ->
            TeamView

        2 ->
            HistoryView

        _ ->
            DraftView


tabViewToInt : TabView -> Int
tabViewToInt view =
    case view of
        DraftView ->
            0

        TeamView ->
            1

        HistoryView ->
            2



-- UPDATE


type Msg
    = Draft Player
    | FlipOrder
    | UndoDraft
    | Reset
    | ChangeView TabView
    | ResortPlayers PlayerSort
    | MoveTeamUp Team
    | MoveTeamDown Team


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Draft player ->
                    draftPlayer player model

                FlipOrder ->
                    { model | waitingTeams = List.reverse model.waitingTeams }

                UndoDraft ->
                    undo model

                Reset ->
                    initModel

                ChangeView tabView ->
                    { model | currentView = tabViewToInt tabView }

                ResortPlayers comparer ->
                    { model | undraftedPlayers = List.sortWith comparer model.undraftedPlayers }

                MoveTeamUp team ->
                    moveTeamUp team model

                MoveTeamDown team ->
                    moveTeamDown team model
    in
        newModel ! []


moveTeamUp : Team -> Model -> Model
moveTeamUp team model =
    let
        teams =
            model.waitingTeams

        update i t =
            if i == team.draftOrder - 1 then
                { t | draftOrder = t.draftOrder + 1 }
            else if i == team.draftOrder then
                { t | draftOrder = t.draftOrder - 1 }
            else
                t

        updatedTeams =
            List.indexedMap update teams
    in
        if team.draftOrder == 0 then
            model
        else
            { model | waitingTeams = Teams.sortedTeams updatedTeams }


moveTeamDown : Team -> Model -> Model
moveTeamDown team model =
    let
        teams =
            model.waitingTeams

        update i t =
            if i == team.draftOrder + 1 then
                { t | draftOrder = t.draftOrder - 1 }
            else if i == team.draftOrder then
                { t | draftOrder = t.draftOrder + 1 }
            else
                t

        updatedTeams =
            List.indexedMap update teams
    in
        if team.draftOrder == List.length model.waitingTeams then
            model
        else
            { model | waitingTeams = Teams.sortedTeams updatedTeams }


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

        round =
            if List.isEmpty newDrafted then
                model.round + 1
            else
                model.round
    in
        { model
            | undraftedPlayers = remaining
            , draftedPlayers = ( player, draftingTeamName ) :: model.draftedPlayers
            , waitingTeams = newWaiting
            , draftedTeams = newDrafted
            , round = round
        }


dummyPlayer : Player
dummyPlayer =
    { firstName = "first"
    , lastName = "last"
    , gender = "x"
    , rating = 1
    }


dummyTeam : Team
dummyTeam =
    { gm = "gm"
    , name = "team"
    , players = []
    , draftOrder = 0
    }


undoDraft : Model -> Model
undoDraft model =
    let
        shouldUndoRound =
            model.round > 1 && List.isEmpty model.draftedTeams

        teamList =
            if shouldUndoRound then
                model.waitingTeams
            else
                model.draftedTeams

        lastDraftedTeam =
            List.head teamList
                |> Maybe.withDefault dummyTeam

        lastTeam =
            Debug.log "lastTeam"
                { lastDraftedTeam
                    | players = List.tail lastDraftedTeam.players |> Maybe.withDefault []
                }

        teamsWaiting =
            if shouldUndoRound then
                [ lastTeam ]
            else
                lastTeam :: model.waitingTeams

        teamsDrafted =
            List.tail teamList |> Maybe.withDefault []

        ( playersWaiting, playersDrafted ) =
            undraftPlayer model

        round =
            if shouldUndoRound then
                model.round - 1
            else
                model.round
    in
        { model
            | undraftedPlayers = playersWaiting
            , draftedPlayers = playersDrafted
            , waitingTeams = teamsWaiting
            , draftedTeams = teamsDrafted
            , round = round
        }


undraftPlayer : Model -> ( List Player, List ( Player, String ) )
undraftPlayer model =
    let
        ( lastPlayer, _ ) =
            List.head model.draftedPlayers
                |> Maybe.withDefault ( dummyPlayer, "" )

        playersWaiting =
            lastPlayer :: model.undraftedPlayers

        playersDrafted =
            List.tail model.draftedPlayers |> Maybe.withDefault []
    in
        ( playersWaiting, playersDrafted )


undo : Model -> Model
undo model =
    if List.isEmpty model.draftedPlayers then
        model
    else
        undoDraft model


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
        :: viewTabNav (tabViewFromInt model.currentView)
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
            , button [ onClick UndoDraft ] [ text "Undo" ]
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
        1 ->
            viewDraftComplete model

        2 ->
            viewDraftHistory model

        _ ->
            viewDraftContent model


viewDraftContent : Model -> List (Html Msg)
viewDraftContent model =
    if List.isEmpty model.undraftedPlayers then
        viewDraftComplete model
    else
        viewDraftInProgress model


viewDraftInProgress : Model -> List (Html Msg)
viewDraftInProgress model =
    [ viewWaitingTeams model.waitingTeams

    --, viewPlayerList "Players" draftablePlayer model.undraftedPlayers
    , viewUndraftedPlayerList model.undraftedPlayers

    --, viewTeamsLastDrafted (viewRound model) model.draftedTeams
    , viewTeamsWithLatest model.round model.draftedTeams
    ]


viewDraftComplete : Model -> List (Html Msg)
viewDraftComplete model =
    let
        teams =
            model.draftedTeams ++ model.waitingTeams

        swap team =
            div []
                [ viewTeamWithRoster True team
                , button [ onClick (MoveTeamUp team) ]
                    [ text "[up]" ]
                , button
                    [ onClick (MoveTeamDown team) ]
                    [ text "[down]" ]
                ]

        teamDisplay =
            if List.length model.draftedPlayers == 0 then
                List.map swap teams
            else
                List.map (viewTeamWithRoster True) teams

        restartButton =
            div [ class "restartDraft" ]
                [ button [ onClick Reset ] [ text "Restart Draft" ]
                ]
    in
        [ div [ id "draftResults" ] teamDisplay
        , restartButton
        ]


viewDraftHistory : Model -> List (Html Msg)
viewDraftHistory model =
    [ div [ id "historyView" ]
        [ viewPlayerList "Draft History" viewDraftedPlayer model.draftedPlayers
        , viewPlayerList "Draft Order" viewDraftedPlayer (List.reverse model.draftedPlayers)
        ]
    ]


viewTeamsWithLatest : Int -> List Team -> Html Msg
viewTeamsWithLatest round teams =
    let
        formatPlayer player =
            let
                className =
                    Players.className player
            in
                dd [ class className ] [ text (Players.playerName player) ]

        title =
            "Round " ++ (toString round)

        viewPlayers team =
            case List.head team.players of
                Just player ->
                    dt [] [ text team.gm, formatPlayer player ]

                Nothing ->
                    text title

        teamList =
            List.map viewPlayers teams
    in
        segment title "latest" [ dl [] teamList ]


viewTeamWithRoster : Bool -> Team -> Html Msg
viewTeamWithRoster format team =
    let
        viewPlayers =
            List.reverse team.players
                |> List.map (viewPlayerDetail [] False)

        ( start, end ) =
            if format then
                ( h3 [] [ text team.gm ], [ br [] [], br [] [] ] )
            else
                ( text "", [] )
    in
        start
            :: [ ul [ class "players" ] viewPlayers ]
            ++ end
            |> div [ class "team" ]


viewPlayerDetail : List (Attribute Msg) -> Bool -> Player -> Html Msg
viewPlayerDetail attributes details player =
    let
        content =
            if details then
                [ text (Players.playerName player)
                , span [ class "stat" ] [ text (Format.formatRating player.rating) ]
                ]
            else
                [ text (Players.playerName player) ]
    in
        li ([ class (Players.className player) ] ++ attributes) content


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
            |> segment title "current"


viewUndraftedPlayerList : List Player -> Html Msg
viewUndraftedPlayerList list =
    let
        header =
            h2 []
                [ span
                    [ id "playerSortHeader" ]
                    [ text "Sorting", viewPlayerSortMenu ]
                , text "Players"
                ]
    in
        div
            [ class "segment"
            , class "undrafted"
            ]
            [ header
            , div
                [ class "content" ]
                [ ol [ class "players" ]
                    (List.map draftablePlayer list)
                ]
            ]


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
            , div [ class className ] [ text (Players.playerName player) ]
            ]


viewPlayerSortMenu : Html Msg
viewPlayerSortMenu =
    let
        sortable compare label =
            div []
                [ a [ onClick (ResortPlayers (compareByDesc compare)) ] [ text " ▼ " ]
                , a [ onClick (ResortPlayers (compareByAsc compare)) ] [ text " ▲ " ]
                , text label
                ]
    in
        div [ id "playerSort" ]
            [ sortable .lastName "Last Name"
            , sortable .firstName "First Name"
            , sortable .gender "Gender"
            , sortable .rating "Rating"
            ]


draftablePlayer : Player -> Html Msg
draftablePlayer player =
    viewPlayerDetail [ class "draftable", onClick (Draft player) ] True player


segment : String -> String -> List (Html Msg) -> Html Msg
segment title className list =
    div
        [ class "segment"
        , class className
        ]
        [ h2 [] [ text title ]
        , div
            [ class "content" ]
            list
        ]
