module Teams exposing (Team, allTeams, decodeTeam, encodeTeam, fullTeamList, sortTeams)

import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)
import Players exposing (Player)


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
        , ( "players", E.list Players.encodePlayer team.players )
        , ( "draftOrder", E.int team.draftOrder )
        ]


allTeams : List Team
allTeams =
    [ { gm = "Rachel Robichaud"
      , players = []
      , draftOrder = 1
      }
    , { gm = "Adam MacDonald"
      , players = []
      , draftOrder = 2
      }
    , { gm = "Kindha Gorman"
      , players = []
      , draftOrder = 3
      }
    , { gm = "Alessandro Colantonio"
      , players = []
      , draftOrder = 4
      }
    , { gm = "Jaime Boss"
      , players = []
      , draftOrder = 5
      }
    , { gm = "Natalie Mullin"
      , players = []
      , draftOrder = 6
      }
    , { gm = "Heather Wallace"
      , players = []
      , draftOrder = 7
      }
    , { gm = "Laura Storey"
      , players = []
      , draftOrder = 8
      }
    , { gm = "Travis Davidson"
      , players = []
      , draftOrder = 9
      }
    , { gm = "Jon Rowe"
      , players = []
      , draftOrder = 10
      }
    ]
