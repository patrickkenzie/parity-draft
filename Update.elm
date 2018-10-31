module Update exposing(..)

import Teams exposing(..)
import Players exposing(..)
import Model exposing(..)

-- UPDATE


type Msg
    = NoOp
    | Draft Player
    | FlipOrder
    | UndoDraft
    | RestartDraft
    | ChangeView TabView
    | ResortPlayers PlayerSort
    | MoveTeamUp Team
    | MoveTeamDown Team
    | ToggleMenu Bool
    | ResetApp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                NoOp ->
                    model

                Draft player ->
                    draftPlayer player model

                FlipOrder ->
                    { model | waitingTeams = List.reverse model.waitingTeams }

                UndoDraft ->
                    undo model

                RestartDraft ->
                    resetDraft model

                ChangeView tabView ->
                    { model | currentView = tabViewToInt tabView }

                ResortPlayers comparer ->
                    { model | undraftedPlayers = List.sortWith comparer model.undraftedPlayers }

                MoveTeamUp team ->
                    moveTeamUp team model

                MoveTeamDown team ->
                    moveTeamDown team model

                ToggleMenu showMenu ->
                    { model | showMenu = showMenu }

                ResetApp ->
                    initModel
    in
        newModel ! []


resetDraft : Model -> Model
resetDraft model =
    let
        resetRoster team =
            { team | players = [] }

        teams =
            List.reverse model.draftedTeams
                ++ model.waitingTeams
                |> List.map resetRoster
                |> Teams.sortedTeams
    in
        { initModel | waitingTeams = teams }


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
        gender =
            player.gender

        remaining =
            List.filter (\p -> p /= player) model.undraftedPlayers

        remainingGender =
            List.filter (\p -> p.gender == gender) remaining

        draftingTeam =
            List.head model.waitingTeams
                |> addPlayer player

        ( newWaiting, newDrafted ) =
            updateRound draftingTeam model

        updatedWaiting =
            if List.isEmpty remainingGender then
                List.reverse newWaiting
            else
                newWaiting

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
            , waitingTeams = updatedWaiting
            , draftedTeams = newDrafted
            , round = round
        }


dummyPlayer : Player
dummyPlayer =
    { firstName = "first"
    , lastName = "last"
    , gender = "x"
    , height = 10
    , rating = 1
    }


dummyTeam : Team
dummyTeam =
    { gm = "gm"
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
