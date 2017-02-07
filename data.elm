module Data exposing (..)

type Gender
    = Female
    | Male


type alias Player =
    { name : String
    , gender : Gender
    , rating : Int
    , salary : Int
    }

type alias Team =
    { gm : String
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

