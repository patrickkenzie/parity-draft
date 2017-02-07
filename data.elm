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
      , name = "Katy Parity"
      , players = []
      }
    , { gm = "Rob"
      , name = "F Bombs"
      , players = []
      }
    , { gm = "Proulx"
      , name = "SOS"
      , players = []
      }
    , { gm = "Owen"
      , name = "Basket"
      , players = []
      }
    , { gm = "Amos"
      , name = "Hindsight Hooligans"
      , players = []
      }
    , { gm = "Wildgen"
      , name = "Kaboom"
      , players = []
      }
    , { gm = "Kells"
      , name = "Mike and the Milburys"
      , players = []
      }
    ]

