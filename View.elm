module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onWithOptions, defaultOptions)
import Format
import Update exposing (..)
import Model exposing (..)
import Players exposing (..)
import Teams exposing (..)
import Json.Decode as Json


-- VIEW


view : Model -> Html Msg
view model =
    title
        :: showMenuButton model.showMenu
        :: viewMenu model.showMenu
        :: viewTabNav (tabViewFromInt model.currentView)
        :: viewTabContent model
        |> div []


title : Html Msg
title =
    h1 [] [ text "Parity Draft" ]


showMenuButton : Bool -> Html Msg
showMenuButton showMenu =
    button
        [ id "menuButton"
        , onClick (ToggleMenu (not showMenu))
        ]
        [ text "Menu" ]


menuItem : Msg -> String -> Html Msg
menuItem msg label =
    li [] [ button [ onClick msg ] [ text label ] ]


viewMenu : Bool -> Html Msg
viewMenu showMenu =
    div
        [ id "menuBackdrop"
        , onClick (ToggleMenu False)
        , hidden (not showMenu)
        ]
        [ div
            [ id "menu"
            , onClickStopPropagation (NoOp)
            ]
            [ h1 [] [ text "Menu" ]
            , ul []
                [ menuItem FlipOrder "Flip Draft Order"
                , menuItem UndoDraft "Undo Player Selection"
                , menuItem RestartDraft "Restart Draft"
                , menuItem ResetApp "Reload Everything"
                ]
            ]
        ]


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    onWithOptions "click" { defaultOptions | stopPropagation = True } (Json.succeed msg)


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
    [ viewWaitingTeams model.draftedTeams model.waitingTeams

    --, viewPlayerList "Players" draftablePlayer model.undraftedPlayers
    , viewUndraftedPlayerList model.playerSearch model.undraftedPlayers

    --, viewTeamsLastDrafted (viewRound model) model.draftedTeams
    , viewTeamsWithLatest model.round model.draftedTeams
    ]


viewDraftComplete : Model -> List (Html Msg)
viewDraftComplete model =
    let
        teams =
            List.reverse model.draftedTeams ++ model.waitingTeams

        viewPreDraft team =
            li []
                [ text team.gm
                , text " ( "
                , a [ onClick (MoveTeamUp team) ]
                    [ text " ▲ " ]
                , a
                    [ onClick (MoveTeamDown team) ]
                    [ text " ▼ " ]
                , text " )"
                ]
    in
        if List.length model.draftedPlayers == 0 then
            [ h2 [] [ text "Draft Order" ]
            , ol [ id "preDraft" ] (List.map viewPreDraft teams)
            ]
        else
            [ div [ id "draftResults" ] (List.map (viewTeamWithRoster True) teams) ]


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
                ( h3 [] [ text team.gm ], [] )
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


viewWaitingTeams : List Team -> List Team -> Html Msg
viewWaitingTeams draftedTeams waitingTeams =
    let
        ( teamDisplay, teamName ) =
            case List.head waitingTeams of
                Just team ->
                    ( viewTeamWithRoster False team, team.gm )

                Nothing ->
                    ( text "no team", "no team" )

        display team =
            li [] [ text team.gm ]

        draftedItems =
            List.map display draftedTeams

        waitingItems =
            case List.tail waitingTeams of
                Just list ->
                    List.map display list

                Nothing ->
                    []

        teamList =
            draftedItems
                ++ [ li [ class "currentTeam" ] [ text teamName ] ]
                ++ waitingItems
                |> ul [ class "teams" ]

        upNext =
            [ h3 [] [ text "Draft Order" ], teamList ]
    in
        teamDisplay
            :: upNext
            |> segment ("Drafting: " ++ teamName) "current"


viewUndraftedPlayerList : String -> List Player -> Html Msg
viewUndraftedPlayerList search fullList =
    let
        females =
            List.filter (\p -> p.gender == Female) fullList

        matches player =
            String.contains (String.toUpper search) (String.toUpper (Players.playerName player))

        players =
            List.filter matches
                (if List.isEmpty females then
                    fullList
                 else
                    females
                )

        header =
            h2 [] [ text "Players" ]

        playerSearch =
            input
                [ type_ "search"
                , placeholder "search players..."
                , value search
                , autofocus True
                , onInput SearchPlayer
                ]
                []
    in
        div
            [ class "segment"
            , class "undrafted"
            ]
            [ header
            , div
                [ class "playerConfig" ]
                [ playerSearch
                , span [ id "playerSortHeader" ] [ text "[Change Sorting]", viewPlayerSortMenu ]
                ]
            , div
                [ class "content" ]
                [ ol [ class "players" ] (List.map draftablePlayer players) ]
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
