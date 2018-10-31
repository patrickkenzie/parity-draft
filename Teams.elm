module Teams exposing (Team, sortTeams, fullTeamList)

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
