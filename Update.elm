module Update exposing (..)

import Teams exposing (..)
import Players exposing (..)
import Model exposing (..)
import Navigation exposing (Location)
import Json.Decode exposing (string)
import Http


-- UPDATE


type Msg
    = NoOp
    | LocalMsg LocalMsg
    | Draft Player
    | FlipOrder
    | UndoDraft
    | RestartDraft
    | ResortPlayers PlayerSort
    | MoveTeamUp Team
    | MoveTeamDown Team
    | ResetApp
    | RequestModelUpdate
    | LoadModelUpdate (Result Http.Error Model)


type LocalMsg
    = ChangeView TabView
    | OnLocationChange Location
    | SearchPlayer String
    | ToggleMenu Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update rawMsg model =
    let
        msg =
            case model.localState.hostingType of
                View _ ->
                    allowReadonlyMessage rawMsg

                _ ->
                    rawMsg

    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            LocalMsg localMsg ->
                ( { model | localState = updateLocalMsg localMsg model.localState }, Cmd.none )

            Draft player ->
                ( draftPlayer player model, uploadModel model )

            FlipOrder ->
                ( { model | waitingTeams = List.reverse model.waitingTeams }, uploadModel model )

            UndoDraft ->
                ( undo model, uploadModel model )

            RestartDraft ->
                ( resetDraft model, uploadModel model )

            ResortPlayers comparer ->
                ( { model | undraftedPlayers = List.sortWith comparer model.undraftedPlayers }, Cmd.none )

            MoveTeamUp team ->
                ( moveTeamUp team model, uploadModel model )

            MoveTeamDown team ->
                ( moveTeamDown team model, uploadModel model )

            ResetApp ->
                ( initModel model.localState, uploadModel model )

            RequestModelUpdate ->
                ( model, loadModel model.localState )

            LoadModelUpdate modelResult ->
                ( case modelResult of
                    Ok m ->
                        m

                    Err e ->
                        (Debug.log (toString e)) model, Cmd.none )


updateLocalMsg : LocalMsg -> LocalState -> LocalState
updateLocalMsg msg state =
    case msg of
        ChangeView tabView ->
            { state | currentView = tabView }

        OnLocationChange location ->
            { state | hostingType = (parseLocation location) }

        SearchPlayer search ->
            { state | playerSearch = search }

        ToggleMenu showMenu ->
            { state | showMenu = showMenu }


resetDraft : Model -> Model
resetDraft model =
    let
        resetRoster team =
            { team | players = [] }

        teams =
            List.reverse model.draftedTeams
                ++ model.waitingTeams
                |> List.map resetRoster
                |> Teams.sortTeams

        newModel =
            initModel model.localState
    in
        { newModel | waitingTeams = teams }


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
            { model | waitingTeams = Teams.sortTeams updatedTeams }


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
            { model | waitingTeams = Teams.sortTeams updatedTeams }


draftPlayer : Player -> Model -> Model
draftPlayer player model =
    let
        playerName =
            (Players.playerName player)

        draftingTeam =
            Maybe.withDefault dummyTeam (List.head model.waitingTeams)

        gms =
            List.map .gm Teams.fullTeamList
    in
        if (List.member playerName gms) && (draftingTeam.gm /= playerName) then
            model
        else
            assignDraftedPlayer player model


assignDraftedPlayer : Player -> Model -> Model
assignDraftedPlayer player model =
    let
        remaining =
            List.filter (\p -> p /= player) model.undraftedPlayers

        remainingGender =
            List.filter (Players.isGender player.gender) remaining

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
    , gender = Female
    , height = 10
    , rating = 1
    }


dummyTeam : Team
dummyTeam =
    { gm = "gm"
    , players = []
    , draftOrder = 0
    }


unFlipDraftOrderIfRequired : Model -> List Team -> List Team
unFlipDraftOrderIfRequired model teams =
    let
        multiGender ps =
            let
                gens =
                    List.map .gender ps
            in
                List.member Female gens && List.member Male gens

        draftedPlayers =
            List.map Tuple.first model.draftedPlayers
    in
        if multiGender draftedPlayers || multiGender model.undraftedPlayers then
            teams
        else
            List.reverse teams


undoDraft : Model -> Model
undoDraft model =
    let
        shouldUndoRound =
            model.round > 1 && List.isEmpty model.draftedTeams

        teamList =
            unFlipDraftOrderIfRequired model
                (if shouldUndoRound then
                    model.waitingTeams
                 else
                    model.draftedTeams
                )

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
        playersWaiting =
            case List.head model.draftedPlayers of
                Just ( player, _ ) ->
                    player :: model.undraftedPlayers

                Nothing ->
                    model.undraftedPlayers

        playersDrafted =
            Maybe.withDefault [] ( List.tail model.draftedPlayers )
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
            Maybe.withDefault [] (List.tail model.waitingTeams)

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


allowReadonlyMessage : Msg -> Msg
allowReadonlyMessage m =
    case m of
        LocalMsg _ ->
            m

        ResortPlayers _ ->
            m

        RequestModelUpdate ->
            m

        LoadModelUpdate _ ->
            m

        _ ->
            (Debug.log ("Blocking message: " ++ (toString m))) NoOp


draftUrl : String -> String
draftUrl draftId =
    "https://paritydraft.patrickkenzie.com/draft/" ++ draftId


loadModel : LocalState -> Cmd Msg
loadModel state =
    case state.hostingType of
        Host _ ->
            Cmd.none

        View id ->
            Http.send LoadModelUpdate
                (Http.get (draftUrl id) (modelDecoder state))

        Local ->
            Cmd.none


uploadModel : Model -> Cmd Msg
uploadModel model =
    case model.localState.hostingType of
        Host id ->
            Http.send (always NoOp)
                (Http.post (draftUrl id) (Http.jsonBody (encodeModel model)) string)

        View _ ->
            Cmd.none

        Local ->
            Cmd.none
