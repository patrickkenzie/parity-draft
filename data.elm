module Data exposing (..)


type alias Player =
    String


type alias Team =
    { gm : Player
    , name : String
    , players : List Player
    }


teams : List Team
teams =
    [ { gm = "Boss"
      , name = "Like a Boss"
      , players = []
      }
    , { gm = "Kindha"
      , name = "Katies"
      , players = []
      }
    , { gm = "Rob"
      , name = "Bombs"
      , players = []
      }
    , { gm = "Proulx"
      , name = "SOS"
      , players = []
      }
    , { gm = "Owen"
      , name = "Beardly"
      , players = []
      }
    , { gm = "Amos"
      , name = "Pretty Flair"
      , players = []
      }
    , { gm = "Wildgen"
      , name = "Ageless"
      , players = []
      }
    , { gm = "Kells"
      , name = "Shoe Beatings"
      , players = []
      }
    ]


players : List Player
players =
    [ "That Guy"
    , "Also Her"
    ]
        ++ List.map (\n -> "Player " ++ toString n) (List.range 1 30)
