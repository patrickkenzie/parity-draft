module Teams exposing (..)

import Players exposing (Player)
import Json.Encode as E exposing (..)
import Json.Decode as D exposing (..)


type alias Team =
    { gm : String
    , players : List Player
    , draftOrder : Int
    }


sortTeams : List Team -> List Team
sortTeams teams =
    List.sortBy .draftOrder teams


fullTeamList : List Team
fullTeamList =
    sortTeams allTeams


decodeTeam : Decoder Team
decodeTeam =
    D.map3 Team
        (field "gm" D.string)
        (field "players" (D.list Players.decodePlayer))
        (field "draftOrder" D.int)


encodeTeam : Team -> E.Value
encodeTeam team =
    E.object
        [ ( "gm", E.string team.gm )
        , ( "players", E.list (List.map Players.encodePlayer team.players) )
        , ( "draftOrder", E.int team.draftOrder )
        ]


allTeams : List Team
allTeams =
    [ { gm = "Michelle Warren"
      , players = []
      , draftOrder = 1
      }
    , { gm = "Kindha Gorman"
      , players = []
      , draftOrder = 2
      }
    , { gm = "Jaime Boss"
      , players = []
      , draftOrder = 3
      }
    , { gm = "Jessie Robinson"
      , players = []
      , draftOrder = 4
      }
    , { gm = "Heather Wallace"
      , players = []
      , draftOrder = 5
      }
    , { gm = "Kate Cavallaro"
      , players = []
      , draftOrder = 6
      }
    , { gm = "Katie Wood"
      , players = []
      , draftOrder = 7
      }
    , { gm = "Andrea Proulx"
      , players = []
      , draftOrder = 8
      }
    , { gm = "Kate Achtell"
      , players = []
      , draftOrder = 9
      }
    , { gm = "Laura Storey"
      , players = []
      , draftOrder = 10
      }
    ]
