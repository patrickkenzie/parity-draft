module Teams exposing (..)

import Players exposing (Player)


type alias Team =
    { gm : String
    , players : List Player
    , draftOrder : Int
    }


sortedTeams : List Team -> List Team
sortedTeams teams =
    List.sortBy .draftOrder teams


teams : List Team
teams =
    sortedTeams allTeams


allTeams : List Team
allTeams =
    [ { gm = "Andrea Proulx"
      , players = []
      , draftOrder = 0
      }
    , { gm = "Heather Wallace"
      , players = []
      , draftOrder = 1
      }
    , { gm = "Jaime Boss"
      , players = []
      , draftOrder = 2
      }
    , { gm = "Jessie Robinson"
      , players = []
      , draftOrder = 3
      }
    , { gm = "Kate Achtell"
      , players = []
      , draftOrder = 4
      }
    , { gm = "Katie Wood"
      , players = []
      , draftOrder = 5
      }
    , { gm = "Kate Cav"
      , players = []
      , draftOrder = 6
      }
    , { gm = "Kindha Gorman"
      , players = []
      , draftOrder = 7
      }
    , { gm = "Laura Storey"
      , players = []
      , draftOrder = 8
      }
    , { gm = "Michelle Warren"
      , players = []
      , draftOrder = 9
      }
    ]
