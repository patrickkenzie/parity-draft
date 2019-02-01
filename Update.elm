module Update exposing (..)

import Teams exposing (..)
import Players exposing (..)
import List.Extra exposing (unique)
import Model exposing (..)
import Navigation exposing (Location)
import Json.Decode exposing (string)
import Random exposing (Seed, generate)
import Random.List exposing (shuffle)
import Http


-- UPDATE


type Msg
    = NoOp
    | LocalMsg LocalMsg
    | Draft Player
    | FlipOrder
    | UndoDraft
    | RestartDraft
    | MoveTeamUp Team
    | MoveTeamDown Team
    | RandomizeDraftOrder
    | ResetApp
    | UpdateDraftOrder (List Team)


type LocalMsg
    = ChangeView TabView
    | LoadModelUpdate (Result Http.Error Model)
    | OnLocationChange Location
    | RequestModelUpdate
    | ResortPlayers PlayerSortEntry
    | SearchPlayer String
    | ToggleMenu Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update rawMsg model =
    let
        msg =
            case model.localState.hostingType of
                View _ ->
                    case rawMsg of
                        LocalMsg _ ->
                            rawMsg

                        _ ->
                            (Debug.log ("Blocking message: " ++ (toString rawMsg))) NoOp

                _ ->
                    rawMsg

        closeMenu model =
            Tuple.first (updateLocalMsg (ToggleMenu False) model)

    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            LocalMsg localMsg ->
                updateLocalMsg localMsg model

            Draft player ->
                uploadModel (draftPlayer player model)

            FlipOrder ->
                uploadModel (closeMenu { model | waitingTeams = List.reverse model.waitingTeams })

            UndoDraft ->
                uploadModel (undo model)

            RestartDraft ->
                uploadModel (closeMenu (resetDraft model))

            MoveTeamUp team ->
                uploadModel (moveTeamUp team model)

            MoveTeamDown team ->
                uploadModel (moveTeamDown team model)

            RandomizeDraftOrder ->
                ( model, generate UpdateDraftOrder (shuffle model.waitingTeams) )

            ResetApp ->
                uploadModel (closeMenu (initModel model.localState))

            UpdateDraftOrder teams ->
                uploadModel (closeMenu (updateDraftOrder teams model))


updateLocalMsg : LocalMsg -> Model -> ( Model, Cmd Msg )
updateLocalMsg msg model =
    let
        localState =
            model.localState

        updateLocal state =
            ( { model | localState = state }, Cmd.none )
    in
        case msg of
            ChangeView tabView ->
                updateLocal { localState | currentView = tabView }

            LoadModelUpdate modelResult ->
                ( Result.withDefault model modelResult, Cmd.none )

            OnLocationChange location ->
                updateLocal { localState | hostingType = (parseLocation location) }

            RequestModelUpdate ->
                ( model, loadModel localState )

            ResortPlayers entry ->
                updateLocal { localState | playerSorts = List.Extra.uniqueBy .tag (entry :: localState.playerSorts) }

            SearchPlayer search ->
                updateLocal { localState | playerSearch = search }

            ToggleMenu showMenu ->
                updateLocal { localState | showMenu = showMenu }


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

        players =
            Players.buildPlayerList (List.length teams) (model.undraftedPlayers ++ (List.map Tuple.first model.draftedPlayers))

        newModel =
            initModel model.localState
    in
        { newModel
        | waitingTeams = teams
        , undraftedPlayers = players
        }


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


updateDraftOrder : List Team -> Model -> Model
updateDraftOrder teams model =
    let
        update i t =
            { t | draftOrder = i }
    in
        { model | waitingTeams = List.indexedMap update teams }


draftPlayer : Player -> Model -> Model
draftPlayer player model =
    let
        playerName =
            (Players.playerName player)

        draftingGM =
            List.head model.waitingTeams
                |> Maybe.map .gm
                |> Maybe.withDefault ""

        gms =
            List.map .gm Teams.fullTeamList
    in
        if (List.member playerName gms) && (draftingGM /= playerName) then
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
                case List.head newWaiting of
                    Just team ->
                        if team.draftOrder == 1 then
                            List.reverse newWaiting
                        else
                            newWaiting

                    Nothing ->
                        newWaiting
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
                |> Maybe.withDefault
                    { gm = "gm"
                    , players = []
                    , draftOrder = 0
                    }

        remainingPlayers =
            Maybe.withDefault [] (List.tail lastDraftedTeam.players)

        lastTeam =
            { lastDraftedTeam
                | players = remainingPlayers
            }

        teamsWaiting =
            if shouldUndoRound then
                [ lastTeam ]
            else
                lastTeam :: model.waitingTeams

        teamsDrafted =
            Maybe.withDefault [] (List.tail teamList)

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
            Maybe.withDefault [] (List.tail model.draftedPlayers)
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


draftUrl : String -> String
draftUrl draftId =
    "https://paritydraft.patrickkenzie.com/draft/" ++ draftId


loadModel : LocalState -> Cmd Msg
loadModel state =
    case state.hostingType of
        Host _ ->
            Cmd.none

        View id ->
            Http.send (LocalMsg << LoadModelUpdate)
                (Http.get (draftUrl id) (modelDecoder state))

        Local ->
            Cmd.none


uploadModel : Model -> ( Model, Cmd Msg )
uploadModel model =
    let cmd =
        case model.localState.hostingType of
            Host id ->
                Http.send (always NoOp)
                    (Http.post (draftUrl id) (Http.jsonBody (encodeModel model)) string)

            View _ ->
                Cmd.none

            Local ->
                Cmd.none
    in
        ( model, cmd )
