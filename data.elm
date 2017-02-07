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
    [ { gm = "Jaime Boss"
      , name = "Like a Boss"
      , players = []
      }
    , { gm = "Kindha Gorman"
      , name = "Katy Parity"
      , players = []
      }
    , { gm = "Rob Ives"
      , name = "F Bombs"
      , players = []
      }
    , { gm = "Andrea Proulx"
      , name = "SOS"
      , players = []
      }
    , { gm = "Owen Lumley"
      , name = "Basket"
      , players = []
      }
    , { gm = "Amos Lee"
      , name = "Hindsight Hooligans"
      , players = []
      }
    , { gm = "Jamie Wildgen"
      , name = "Kaboom"
      , players = []
      }
    , { gm = "Brian Kells"
      , name = "Mike and the Milburys"
      , players = []
      }
    ]

