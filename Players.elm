module Players exposing (..)


type Gender
    = Female
    | Male


type alias Player =
    { firstName : String
    , lastName : String
    , gender : Gender
    , height : Int
    , rating : Int
    }


className : Player -> String
className player =
    if player.gender == Female then
        "female"
    else
        "male"


playerName : Player -> String
playerName player =
    player.firstName ++ " " ++ player.lastName


sortPlayers : (Player -> comparable) -> List Player -> List Player
sortPlayers sorter players =
    List.sortBy sorter allPlayers


players : List Player
players =
    sortPlayers .lastName allPlayers


allPlayers : List Player
allPlayers =
    [ { firstName = "Hannah"
      , lastName = "Dawson"
      , gender = Female
      , height = 62
      , rating = 10
      }
    , { firstName = "Alisha"
      , lastName = "Zhao"
      , gender = Female
      , height = 64
      , rating = 10
      }
    , { firstName = "Cassie"
      , lastName = "Berquist"
      , gender = Female
      , height = 66
      , rating = 9
      }
    , { firstName = "Dominique"
      , lastName = "Rioux"
      , gender = Female
      , height = 66
      , rating = 9
      }
    , { firstName = "Josee"
      , lastName = "Guibord"
      , gender = Female
      , height = 64
      , rating = 9
      }
    , { firstName = "Kristie"
      , lastName = "Ellis"
      , gender = Female
      , height = 66
      , rating = 9
      }
    , { firstName = "Andrea"
      , lastName = "Proulx"
      , gender = Female
      , height = 62
      , rating = 8
      }
    , { firstName = "Jaime"
      , lastName = "Boss"
      , gender = Female
      , height = 66
      , rating = 8
      }
    , { firstName = "Jessie"
      , lastName = "Robinson"
      , gender = Female
      , height = 65
      , rating = 8
      }
    , { firstName = "Vanessa"
      , lastName = "Mann"
      , gender = Female
      , height = 62
      , rating = 8
      }
    , { firstName = "Sherri"
      , lastName = "Ross"
      , gender = Female
      , height = 65
      , rating = 7
      }
    , { firstName = "An"
      , lastName = "Tran"
      , gender = Female
      , height = 64
      , rating = 7
      }
    , { firstName = "Justine"
      , lastName = "Price"
      , gender = Female
      , height = 63
      , rating = 7
      }
    , { firstName = "Susan"
      , lastName = "Sunde"
      , gender = Female
      , height = 61
      , rating = 7
      }
    , { firstName = "Angela"
      , lastName = "Mueller"
      , gender = Female
      , height = 69
      , rating = 7
      }
    , { firstName = "Laura"
      , lastName = "Chambers Storey"
      , gender = Female
      , height = 68
      , rating = 7
      }
    , { firstName = "Kate"
      , lastName = "Achtell"
      , gender = Female
      , height = 61
      , rating = 6
      }
    , { firstName = "Lauren"
      , lastName = "Ellis"
      , gender = Female
      , height = 63
      , rating = 6
      }
    , { firstName = "Stephanie"
      , lastName = "Verbit"
      , gender = Female
      , height = 64
      , rating = 6
      }
    , { firstName = "Stacey"
      , lastName = "Wowchuk"
      , gender = Female
      , height = 62
      , rating = 6
      }
    , { firstName = "Melissa"
      , lastName = "Jess"
      , gender = Female
      , height = 64
      , rating = 6
      }
    , { firstName = "Nicole"
      , lastName = "MacDonald"
      , gender = Female
      , height = 66
      , rating = 6
      }
    , { firstName = "Carrie-Anne"
      , lastName = "Whyte"
      , gender = Female
      , height = 65
      , rating = 6
      }
    , { firstName = "Neena"
      , lastName = "Sidhu"
      , gender = Female
      , height = 63
      , rating = 6
      }
    , { firstName = "Heather"
      , lastName = "Wallace"
      , gender = Female
      , height = 64
      , rating = 6
      }
    , { firstName = "Christine"
      , lastName = "Beals"
      , gender = Female
      , height = 62
      , rating = 5
      }
    , { firstName = "Marie-Ange"
      , lastName = "Gravel"
      , gender = Female
      , height = 66
      , rating = 5
      }
    , { firstName = "Hope"
      , lastName = "Celani"
      , gender = Female
      , height = 62
      , rating = 5
      }
    , { firstName = "Celine"
      , lastName = "Dumais"
      , gender = Female
      , height = 67
      , rating = 4
      }
    , { firstName = "Katherine"
      , lastName = "Matheson"
      , gender = Female
      , height = 66
      , rating = 4
      }
    , { firstName = "Alix"
      , lastName = "Ranger"
      , gender = Female
      , height = 67
      , rating = 4
      }
    , { firstName = "Kristyn"
      , lastName = "Berquist"
      , gender = Female
      , height = 64
      , rating = 2
      }
    , { firstName = "Greg"
      , lastName = "Ellis"
      , gender = Male
      , height = 71
      , rating = 10
      }
    , { firstName = "Frederic"
      , lastName = "Caron"
      , gender = Male
      , height = 71
      , rating = 9
      }
    , { firstName = "Tom"
      , lastName = "Newman"
      , gender = Male
      , height = 74
      , rating = 9
      }
    , { firstName = "Martin"
      , lastName = "Cloake"
      , gender = Male
      , height = 79
      , rating = 9
      }
    , { firstName = "Will"
      , lastName = "Leckie"
      , gender = Male
      , height = 74
      , rating = 8
      }
    , { firstName = "Chris"
      , lastName = "Sullivan"
      , gender = Male
      , height = 72
      , rating = 8
      }
    , { firstName = "Marcus"
      , lastName = "Bordage"
      , gender = Male
      , height = 72
      , rating = 8
      }
    , { firstName = "Christopher"
      , lastName = "Keates"
      , gender = Male
      , height = 75
      , rating = 8
      }
    , { firstName = "Craig"
      , lastName = "Anderson"
      , gender = Male
      , height = 67
      , rating = 8
      }
    , { firstName = "Geofford"
      , lastName = "Seaborn"
      , gender = Male
      , height = 75
      , rating = 8
      }
    , { firstName = "Ken"
      , lastName = "Maclean"
      , gender = Male
      , height = 70
      , rating = 8
      }
    , { firstName = "Ryan"
      , lastName = "Briggs"
      , gender = Male
      , height = 66
      , rating = 8
      }
    , { firstName = "Travis"
      , lastName = "Davidson"
      , gender = Male
      , height = 72
      , rating = 8
      }
    , { firstName = "Brian"
      , lastName = "Kells"
      , gender = Male
      , height = 72
      , rating = 8
      }
    , { firstName = "Hadrian"
      , lastName = "Mertins - Kirkwood"
      , gender = Male
      , height = 73
      , rating = 8
      }
    , { firstName = "Scott"
      , lastName = "Higgins"
      , gender = Male
      , height = 72
      , rating = 7
      }
    , { firstName = "Adam"
      , lastName = "MacDonald"
      , gender = Male
      , height = 68
      , rating = 7
      }
    , { firstName = "Stephen"
      , lastName = "Close"
      , gender = Male
      , height = 71
      , rating = 7
      }
    , { firstName = "Nicholas"
      , lastName = "Aghajanian"
      , gender = Male
      , height = 68
      , rating = 7
      }
    , { firstName = "Mehmet"
      , lastName = "Karman"
      , gender = Male
      , height = 72
      , rating = 7
      }
    , { firstName = "Greg"
      , lastName = "Probe"
      , gender = Male
      , height = 70
      , rating = 7
      }
    , { firstName = "Brian"
      , lastName = "Perry"
      , gender = Male
      , height = 72
      , rating = 7
      }
    , { firstName = "Jamie"
      , lastName = "Wildgen"
      , gender = Male
      , height = 72
      , rating = 7
      }
    , { firstName = "Kelsey"
      , lastName = "Charie"
      , gender = Male
      , height = 67
      , rating = 7
      }
    , { firstName = "John"
      , lastName = "Haig"
      , gender = Male
      , height = 71
      , rating = 7
      }
    , { firstName = "Shubho Bo"
      , lastName = "Biswas"
      , gender = Male
      , height = 66
      , rating = 7
      }
    , { firstName = "Alessandro"
      , lastName = "Colantonio"
      , gender = Male
      , height = 70
      , rating = 7
      }
    , { firstName = "Jonathan"
      , lastName = "Pindur"
      , gender = Male
      , height = 72
      , rating = 7
      }
    , { firstName = "Dan"
      , lastName = "Thomson"
      , gender = Male
      , height = 70
      , rating = 7
      }
    , { firstName = "Kevin"
      , lastName = "Hughes"
      , gender = Male
      , height = 70
      , rating = 7
      }
    , { firstName = "Logan"
      , lastName = "Ashall"
      , gender = Male
      , height = 72
      , rating = 6
      }
    , { firstName = "Michael"
      , lastName = "Colantonio"
      , gender = Male
      , height = 70
      , rating = 6
      }
    , { firstName = "Will"
      , lastName = "Reid"
      , gender = Male
      , height = 67
      , rating = 6
      }
    , { firstName = "Wing-Leung"
      , lastName = "Chan"
      , gender = Male
      , height = 67
      , rating = 6
      }
    , { firstName = "Jeff"
      , lastName = "Hunt"
      , gender = Male
      , height = 69
      , rating = 6
      }
    , { firstName = "Giulian"
      , lastName = "De La Merced"
      , gender = Male
      , height = 67
      , rating = 6
      }
    , { firstName = "Jon"
      , lastName = "Rowe"
      , gender = Male
      , height = 73
      , rating = 6
      }
    , { firstName = "Nick"
      , lastName = "Amlin"
      , gender = Male
      , height = 72
      , rating = 6
      }
    , { firstName = "Liam"
      , lastName = "Parker"
      , gender = Male
      , height = 74
      , rating = 6
      }
    , { firstName = "Trevor"
      , lastName = "Stocki"
      , gender = Male
      , height = 36
      , rating = 6
      }
    , { firstName = "Kevin"
      , lastName = "Barford"
      , gender = Male
      , height = 72
      , rating = 6
      }
    , { firstName = "Jason"
      , lastName = "Fraser"
      , gender = Male
      , height = 70
      , rating = 6
      }
    , { firstName = "Andrew"
      , lastName = "Spearin"
      , gender = Male
      , height = 74
      , rating = 6
      }
    , { firstName = "Richard"
      , lastName = "Gregory"
      , gender = Male
      , height = 66
      , rating = 6
      }
    , { firstName = "Allan"
      , lastName = "Godding"
      , gender = Male
      , height = 75
      , rating = 6
      }
    , { firstName = "Rob"
      , lastName = "Tyson"
      , gender = Male
      , height = 70
      , rating = 6
      }
    , { firstName = "Jim"
      , lastName = "Robinson"
      , gender = Male
      , height = 76
      , rating = 6
      }
    , { firstName = "Nick"
      , lastName = "Klimowicz"
      , gender = Male
      , height = 74
      , rating = 6
      }
    , { firstName = "Derek"
      , lastName = "Tokarski"
      , gender = Male
      , height = 70
      , rating = 6
      }
    , { firstName = "Tim"
      , lastName = "Kealey"
      , gender = Male
      , height = 72
      , rating = 6
      }
    , { firstName = "Sebastien"
      , lastName = "Belanger"
      , gender = Male
      , height = 72
      , rating = 6
      }
    , { firstName = "Patrick"
      , lastName = "Kenzie"
      , gender = Male
      , height = 66
      , rating = 6
      }
    , { firstName = "Nick"
      , lastName = "Theriault"
      , gender = Male
      , height = 72
      , rating = 6
      }
    , { firstName = "Michael"
      , lastName = "Davidson"
      , gender = Male
      , height = 76
      , rating = 6
      }
    , { firstName = "Jonathan"
      , lastName = "Champagne"
      , gender = Male
      , height = 67
      , rating = 5
      }
    , { firstName = "Matthew"
      , lastName = "Schijns"
      , gender = Male
      , height = 72
      , rating = 5
      }
    , { firstName = "Edwin"
      , lastName = "Wong"
      , gender = Male
      , height = 66
      , rating = 5
      }
    , { firstName = "Michael"
      , lastName = "Wong"
      , gender = Male
      , height = 66
      , rating = 5
      }
    , { firstName = "Simon"
      , lastName = "Berry"
      , gender = Male
      , height = 68
      , rating = 5
      }
    , { firstName = "Ben"
      , lastName = "Curran"
      , gender = Male
      , height = 74
      , rating = 5
      }
    , { firstName = "Thomas"
      , lastName = "Sattolo"
      , gender = Male
      , height = 73
      , rating = 5
      }
    , { firstName = "David"
      , lastName = "Townsend"
      , gender = Male
      , height = 71
      , rating = 5
      }
    , { firstName = "Graham"
      , lastName = "Brown"
      , gender = Male
      , height = 70
      , rating = 5
      }
    , { firstName = "Darryl"
      , lastName = "Payne"
      , gender = Male
      , height = 73
      , rating = 4
      }
    ]
