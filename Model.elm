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
    , playerSearch : String
    , localState : LocalState
    }


type TabView
    = DraftView
    | TeamView
    | HistoryView


type HostType
    = Local
    | Host String
    | View String

type alias LocalState =
    { currentView : TabView
    , showMenu : Bool
    , hostingType: HostType
    }

type alias PlayerSort =
    Player -> Player -> Order


initModel : LocalState -> Model
initModel localState =
    { undraftedPlayers = Players.fullPlayerList
    , draftedPlayers = []
    , waitingTeams = Teams.fullTeamList
    , draftedTeams = []
    , round = 1
    , playerSearch = ""
    , localState = localState
    }


matchers : Parser (HostType -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map Local top
        , UrlParser.map Host (s "host" </> UrlParser.string)
        , UrlParser.map View (s "view" </> UrlParser.string)
        ]

--
parseLocation : Location -> HostType
parseLocation location =
    case parseHash matchers location of
        Just route ->
            route

        Nothing ->
            Local


modelDecoder : LocalState -> D.Decoder Model
modelDecoder localState =
    DP.decode Model
        |> required "undraftedPlayers" (D.list Players.decodePlayer)
        |> required "draftedPlayers" (D.list decodeDraftedPlayer)
        |> required "waitingTeams" (D.list Teams.decodeTeam)
        |> required "draftedTeams" (D.list Teams.decodeTeam)
        |> required "round" D.int
        |> required "playerSearch" D.string
        |> hardcoded localState


decodeModel : D.Value -> LocalState -> Maybe Model
decodeModel value localState =
    Result.toMaybe (D.decodeValue (modelDecoder localState) value)


decodeDraftedPlayer : Decoder ( Player, String )
decodeDraftedPlayer =
    D.map2 (,)
        (field "player" Players.decodePlayer)
        (field "team" D.string)


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ( "undraftedPlayers", E.list (List.map Players.encodePlayer model.undraftedPlayers) )
        , ( "draftedPlayers", E.list (List.map encodeDraftedPlayer model.draftedPlayers) )
        , ( "waitingTeams", E.list (List.map Teams.encodeTeam model.waitingTeams) )
        , ( "draftedTeams", E.list (List.map Teams.encodeTeam model.draftedTeams) )
        , ( "round", E.int model.round )
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
