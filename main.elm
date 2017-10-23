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
    | ResortPlayers PlayerSort


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

        ResortPlayers comparer ->
            { model | undraftedPlayers = List.sortWith comparer model.undraftedPlayers }


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

        teamDisplay =
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
                , span [ class "stat" ] [ text (Format.formatHeight player.height) ]
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
            , sortable .height "Height"
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
