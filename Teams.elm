module Teams exposing (..)

import Players exposing (Player)


type alias Team =
    { gm : String
    , name : String
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
    [ { gm = "Chris Sullivan"
      , name = ""
      , players = []
      , draftOrder = 0
      }
    , { gm = "Mehmet Karman"
      , name = ""
      , players = []
      , draftOrder = 1
      }
    , { gm = "Christopher Keates"
      , name = ""
      , players = []
      , draftOrder = 2
      }
    , { gm = "Al Colantonio"
      , name = ""
      , players = []
      , draftOrder = 3
      }
    , { gm = "Cassie Berquist"
      , name = ""
      , players = []
      , draftOrder = 4
      }
    , { gm = "Brian Perry"
      , name = ""
      , players = []
      , draftOrder = 5
      }
    , { gm = "Heather Wallace"
      , name = ""
      , players = []
      , draftOrder = 6
      }
    , { gm = "Jessie Robinson"
      , name = ""
      , players = []
      , draftOrder = 7
      }
    ]
