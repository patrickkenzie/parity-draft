module Model exposing (..)

import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as DP exposing (..)
import Json.Encode as E exposing (..)
import Players exposing (..)
import Teams exposing (..)
import Url exposing (..)
import Url.Parser exposing (..)



-- MODEL


type alias Model =
    { undraftedPlayers : List Player
    , draftedPlayers : List ( Player, String )
    , waitingTeams : List Team
    , draftedTeams : List Team
    , round : Int
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
    , hostingType : HostType
    , playerSearch : String
    , playerSorts : List PlayerSortEntry
    , showMenu : Bool
    }


type alias PlayerSort =
    Player -> Player -> Order


type alias PlayerSortEntry =
    { tag : String
    , sort : PlayerSort
    }


initModel : LocalState -> Model
initModel localState =
    { undraftedPlayers = Players.defaultPlayerList (List.length Teams.fullTeamList)
    , draftedPlayers = []
    , waitingTeams = Teams.fullTeamList
    , draftedTeams = []
    , round = 1
    , localState = localState
    }


matchers : Parser (HostType -> a) a
matchers =
    Url.Parser.oneOf
        [ Url.Parser.map Local top
        , Url.Parser.map Host (s "host" </> Url.Parser.string)
        , Url.Parser.map View (s "view" </> Url.Parser.string)
        ]


parseLocation : Url.Url -> HostType
parseLocation location =
    case Url.Parser.parse matchers location of
        Just route ->
            route

        Nothing ->
            Local


modelDecoder : LocalState -> D.Decoder Model
modelDecoder localState =
    D.succeed Model
        |> required "undraftedPlayers" (D.list Players.decodePlayer)
        |> required "draftedPlayers" (D.list decodeDraftedPlayer)
        |> required "waitingTeams" (D.list Teams.decodeTeam)
        |> required "draftedTeams" (D.list Teams.decodeTeam)
        |> required "round" D.int
        |> hardcoded localState


decodeModel : D.Value -> LocalState -> Maybe Model
decodeModel value localState =
    Result.toMaybe (D.decodeValue (modelDecoder localState) value)


decodeDraftedPlayer : Decoder ( Player, String )
decodeDraftedPlayer =
    D.map2 (\a b -> ( a, b ))
        (field "player" Players.decodePlayer)
        (field "team" D.string)


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ( "undraftedPlayers", E.list Players.encodePlayer model.undraftedPlayers )
        , ( "draftedPlayers", E.list encodeDraftedPlayer model.draftedPlayers )
        , ( "waitingTeams", E.list Teams.encodeTeam model.waitingTeams )
        , ( "draftedTeams", E.list Teams.encodeTeam model.draftedTeams )
        , ( "round", E.int model.round )
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
