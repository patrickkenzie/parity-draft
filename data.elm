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


players : List Player
players =
    [ { name = "That Guy", gender = Male, rating = 7, salary = 1045404 }
    , { name = "Also Her", gender = Female, rating = 6, salary = 1134504 }
    ]
        ++ List.map (\n -> "Player " ++ toString n |> makeMalePlayer) (List.range 1 30)
        ++ List.map (\n -> "Player " ++ toString n |> makeFemalePlayer) (List.range 1 20)


makeMalePlayer : String -> Player
makeMalePlayer name =
    { name = name, gender = Male, rating = 6, salary = 1031459 }


makeFemalePlayer : String -> Player
makeFemalePlayer name =
    { name = name, gender = Female, rating = 6, salary = 1031459 }
