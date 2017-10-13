module Teams exposing (..)

import Players exposing (Player)


type alias Team =
    { gm : String
    , name : String
    , players : List Player
    }


teams : List Team
teams =
    [ { gm = "Alessandro Colantonio"
      , name = ""
      , players = []
      }
    , { gm = "Brian Kells"
      , name = ""
      , players = []
      }
    , { gm = "Cassie Berquist"
      , name = ""
      , players = []
      }
    , { gm = "Christopher Keates"
      , name = ""
      , players = []
      }
    , { gm = "Chris Sullivan"
      , name = ""
      , players = []
      }
    , { gm = "Jessie Robinson"
      , name = ""
      , players = []
      }
    , { gm = "Mehmet Karman"
      , name = ""
      , players = []
      }
    , { gm = "Travis Davidson"
      , name = ""
      , players = []
      }
    ]
