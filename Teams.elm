module Teams exposing (Team, TeamID, allTeams, decodeTeam, encodeTeam, fullTeamList, sortTeams)

import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)
import Players exposing (Player)


type alias TeamID =
    Int


type alias Team =
    { id : TeamID
    , gm : String
    , players : List Player
    , draftOrder : Int
    }


makeTeam : Int -> String -> Team
makeTeam order gm =
    Team order gm [] order


sortTeams : List Team -> List Team
sortTeams teams =
    List.sortBy .draftOrder teams


fullTeamList : List Team
fullTeamList =
    sortTeams allTeams


decodeTeam : Decoder Team
decodeTeam =
    D.map4 Team
        (field "id" D.int)
        (field "gm" D.string)
        (field "players" (D.list Players.decodePlayer))
        (field "draftOrder" D.int)


encodeTeam : Team -> E.Value
encodeTeam team =
    E.object
        [ ( "id", E.int team.id )
        , ( "gm", E.string team.gm )
        , ( "players", E.list Players.encodePlayer team.players )
        , ( "draftOrder", E.int team.draftOrder )
        ]


allTeams : List Team
allTeams =
    List.indexedMap makeTeam gms


gms : List String
gms =
    [ "GM 0"
    , "GM 1"
    , "GM 2"
    , "GM 3"
    , "GM 4"
    , "GM 5"
    , "GM 6"
    , "GM 7"
    , "GM 8"
    , "GM 9"
    ]
