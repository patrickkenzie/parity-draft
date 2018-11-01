module Model exposing (..)

import Players exposing (..)
import Teams exposing (Team)


-- MODEL


type alias Model =
    { undraftedPlayers : List Player
    , draftedPlayers : List ( Player, String )
    , waitingTeams : List Team
    , draftedTeams : List Team
    , round : Int
    , currentView : Int
    , showMenu : Bool
    , playerSearch : String
    , hostingType : String
    , hostingId : String
    }


type TabView
    = DraftView
    | TeamView
    | HistoryView


type alias PlayerSort =
    Player -> Player -> Order


initModel : Model
initModel =
    { undraftedPlayers = Players.fullPlayerList
    , draftedPlayers = []
    , waitingTeams = Teams.fullTeamList
    , draftedTeams = []
    , round = 1
    , currentView = 0
    , showMenu = False
    , playerSearch = ""
    , hostingType = ""
    , hostingId = ""
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
