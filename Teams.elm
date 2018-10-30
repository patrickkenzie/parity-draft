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
    , { gm = "Kate Cav"
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
