module Model exposing (..)

import Players exposing (..)
import Teams exposing (..)
import Navigation exposing (Location)
import UrlParser exposing (..)
import Json.Encode as E exposing (..)
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as DP exposing (..)


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
    UrlParser.oneOf
        [ UrlParser.map Top top
        , UrlParser.map Host (s "host" </> UrlParser.string)
        , UrlParser.map View (s "view" </> UrlParser.string)
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


decodeModel : D.Value -> String -> String -> Maybe Model
decodeModel value hostingType hostingId =
    let decoder =
        DP.decode Model
            |> required "undraftedPlayers" (D.list Players.decodePlayer)
            |> required "draftedPlayers" (D.list decodeDraftedPlayer)
            |> required "waitingTeams" (D.list Teams.decodeTeam)
            |> required "draftedTeams" (D.list Teams.decodeTeam)
            |> required "round" D.int
            |> required "currentView" D.int
            |> required "showMenu" D.bool
            |> required "playerSearch" D.string
            |> hardcoded hostingType
            |> hardcoded hostingId

     in
        Result.toMaybe (D.decodeValue decoder value)


decodeDraftedPlayer : Decoder ( Player, String )
decodeDraftedPlayer =
    D.map2 (,)
        (field "player" Players.decodePlayer)
        (field "team" D.string)


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ("undraftedPlayers", E.list (List.map Players.encodePlayer model.undraftedPlayers) )
        , ( "draftedPlayers", E.list (List.map encodeDraftedPlayer model.draftedPlayers) )
        , ( "waitingTeams", E.list (List.map Teams.encodeTeam model.waitingTeams) )
        , ( "draftedTeams", E.list (List.map Teams.encodeTeam model.draftedTeams) )
        , ( "round", E.int model.round )
        , ( "currentView", E.int model.currentView )
        , ( "showMenu", E.bool model.showMenu )
        , ( "playerSearch", E.string model.playerSearch )
        ]


encodeDraftedPlayer : ( Player, String ) -> E.Value
encodeDraftedPlayer draftedPlayer =
    let
        ( player, team ) =
            draftedPlayer
    in
        E.object
            [ ( "player", Players.encodePlayer player )
            , ( "team", E.string team )
            ]
