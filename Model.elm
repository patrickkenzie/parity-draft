module Model exposing (..)

import Players exposing (..)
import Teams exposing (Team)
import Navigation exposing (Location)
import UrlParser exposing (..)


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


initModel : String -> String -> Model
initModel hostType hostId =
    { undraftedPlayers = Players.fullPlayerList
    , draftedPlayers = []
    , waitingTeams = Teams.fullTeamList
    , draftedTeams = []
    , round = 1
    , currentView = 0
    , showMenu = False
    , playerSearch = ""
    , hostingType = hostType
    , hostingId = hostId
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


type Route
    = Top
    | Host String
    | View String


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Top top
        , map Host (s "host" </> string)
        , map View (s "view" </> string)
        ]


parseLocation : Location -> ( String, String )
parseLocation location =
    case parseHash matchers location of
        Just route ->
            case route of
                Top ->
                    ( "", "" )

                Host id ->
                    ( "host", id )

                View id ->
                    ( "view", id )

        Nothing ->
            ( "", "" )
