module Players exposing (..)


type alias Player =
    { firstName : String
    , lastName : String
    , gender : String
    , rating : Int
    }


className : Player -> String
className player =
    if player.gender == "Female" then
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
    [ { firstName = "Nicholas", lastName = "Aghajanian", gender = "Male", rating = 7 }
    , { firstName = "Nick", lastName = "Amlin", gender = "Male", rating = 6 }
    , { firstName = "Craig", lastName = "Anderson", gender = "Male", rating = 8 }
    , { firstName = "Kevin", lastName = "Barford", gender = "Male", rating = 6 }
    , { firstName = "Sebastien", lastName = "Belanger", gender = "Male", rating = 6 }
    , { firstName = "Simon", lastName = "Berry", gender = "Male", rating = 5 }
    , { firstName = "Shubho Bo", lastName = "Biswas", gender = "Male", rating = 7 }
    , { firstName = "Lance", lastName = "Blackstock", gender = "Male", rating = 10 }
    , { firstName = "Ryan", lastName = "Briggs", gender = "Male", rating = 8 }
    , { firstName = "Graham", lastName = "Brown", gender = "Male", rating = 5 }
    , { firstName = "Frederic", lastName = "Caron", gender = "Male", rating = 9 }
    , { firstName = "Christopher", lastName = "Castonguay", gender = "Male", rating = 7 }
    , { firstName = "Jonathan", lastName = "Champagne", gender = "Male", rating = 5 }
    , { firstName = "Wing-Leung", lastName = "Chan", gender = "Male", rating = 6 }
    , { firstName = "Marc", lastName = "Charette", gender = "Male", rating = 4 }
    , { firstName = "Kelsey", lastName = "Charie", gender = "Male", rating = 7 }
    , { firstName = "Martin", lastName = "Cloake", gender = "Male", rating = 9 }
    , { firstName = "Stephen", lastName = "Close", gender = "Male", rating = 7 }
    , { firstName = "Michael", lastName = "Colantonio", gender = "Male", rating = 6 }
    , { firstName = "Alessandro", lastName = "Colantonio", gender = "Male", rating = 7 }
    , { firstName = "Ben", lastName = "Curran", gender = "Male", rating = 5 }
    , { firstName = "Micheal", lastName = "Davidson", gender = "Male", rating = 3 }
    , { firstName = "Jason", lastName = "Fraser", gender = "Male", rating = 6 }
    , { firstName = "Allan", lastName = "Godding", gender = "Male", rating = 6 }
    , { firstName = "Richard", lastName = "Gregory", gender = "Male", rating = 6 }
    , { firstName = "John", lastName = "Haig", gender = "Male", rating = 7 }
    , { firstName = "Scott", lastName = "Higgins", gender = "Male", rating = 7 }
    , { firstName = "Derek", lastName = "Hodgson", gender = "Male", rating = 8 }
    , { firstName = "Craig", lastName = "Holden", gender = "Male", rating = 6 }
    , { firstName = "Jeff", lastName = "Hunt", gender = "Male", rating = 6 }
    , { firstName = "Rob", lastName = "Ives", gender = "Male", rating = 7 }
    , { firstName = "Mehmet", lastName = "Karman", gender = "Male", rating = 7 }
    , { firstName = "Tim", lastName = "Kealey", gender = "Male", rating = 6 }
    , { firstName = "Christopher", lastName = "Keates", gender = "Male", rating = 8 }
    , { firstName = "Brian", lastName = "Kells", gender = "Male", rating = 8 }
    , { firstName = "Patrick", lastName = "Kenzie", gender = "Male", rating = 6 }
    , { firstName = "Nick", lastName = "Klimowicz", gender = "Male", rating = 6 }
    , { firstName = "Mike", lastName = "Lee", gender = "Male", rating = 10 }
    , { firstName = "Ken", lastName = "Maclean", gender = "Male", rating = 8 }
    , { firstName = "Hadrian", lastName = "Mertins-Kirkwood", gender = "Male", rating = 8 }
    , { firstName = "Tom", lastName = "Newman", gender = "Male", rating = 8 }
    , { firstName = "David", lastName = "O'Connor", gender = "Male", rating = 6 }
    , { firstName = "Brian", lastName = "Perry", gender = "Male", rating = 7 }
    , { firstName = "Jonathan", lastName = "Pindur", gender = "Male", rating = 7 }
    , { firstName = "Benjamin", lastName = "Piper", gender = "Male", rating = 7 }
    , { firstName = "Greg", lastName = "Probe", gender = "Male", rating = 7 }
    , { firstName = "Will", lastName = "Reid", gender = "Male", rating = 6 }
    , { firstName = "Jim", lastName = "Robinson", gender = "Male", rating = 6 }
    , { firstName = "Jon", lastName = "Rowe", gender = "Male", rating = 6 }
    , { firstName = "Thomas", lastName = "Sattolo", gender = "Male", rating = 5 }
    , { firstName = "Matthew", lastName = "Schijns", gender = "Male", rating = 5 }
    , { firstName = "Andre", lastName = "Scott", gender = "Male", rating = 5 }
    , { firstName = "Geofford", lastName = "Seaborn", gender = "Male", rating = 8 }
    , { firstName = "Andrew", lastName = "Spearin", gender = "Male", rating = 6 }
    , { firstName = "Trevor", lastName = "Stocki", gender = "Male", rating = 6 }
    , { firstName = "Chris", lastName = "Sullivan", gender = "Male", rating = 8 }
    , { firstName = "Nick", lastName = "Theriault", gender = "Male", rating = 6 }
    , { firstName = "Dan", lastName = "Thomson", gender = "Male", rating = 7 }
    , { firstName = "David", lastName = "Townsend", gender = "Male", rating = 5 }
    , { firstName = "Jay Thor", lastName = "Turner", gender = "Male", rating = 6 }
    , { firstName = "Rob", lastName = "Tyson", gender = "Male", rating = 6 }
    , { firstName = "Jamie", lastName = "Wildgen", gender = "Male", rating = 7 }
    , { firstName = "Edwin", lastName = "Wong", gender = "Male", rating = 5 }
    , { firstName = "Michael", lastName = "Wong", gender = "Male", rating = 5 }
    , { firstName = "Kate", lastName = "Achtell", gender = "Female", rating = 6 }
    , { firstName = "Christine", lastName = "Beals", gender = "Female", rating = 5 }
    , { firstName = "Kristyn", lastName = "Berquist", gender = "Female", rating = 2 }
    , { firstName = "Cassie", lastName = "Berquist", gender = "Female", rating = 9 }
    , { firstName = "Jaime", lastName = "Boss", gender = "Female", rating = 8 }
    , { firstName = "Melany", lastName = "Bouchard", gender = "Female", rating = 8 }
    , { firstName = "Hope", lastName = "Celani", gender = "Female", rating = 5 }
    , { firstName = "Laura", lastName = "Chambers Storey", gender = "Female", rating = 7 }
    , { firstName = "Celine", lastName = "Dumais", gender = "Female", rating = 4 }
    , { firstName = "Kristie", lastName = "Ellis", gender = "Female", rating = 8 }
    , { firstName = "Tanya", lastName = "Gallant", gender = "Female", rating = 5 }
    , { firstName = "Clare", lastName = "Gee", gender = "Female", rating = 5 }
    , { firstName = "Marie-Ange", lastName = "Gravel", gender = "Female", rating = 5 }
    , { firstName = "Josee", lastName = "Guibord", gender = "Female", rating = 9 }
    , { firstName = "Melissa", lastName = "Jess", gender = "Female", rating = 6 }
    , { firstName = "Adrienne", lastName = "Junek", gender = "Female", rating = 6 }
    , { firstName = "Ashlin", lastName = "Kelly", gender = "Female", rating = 7 }
    , { firstName = "Vanessa", lastName = "Mann", gender = "Female", rating = 8 }
    , { firstName = "Angela", lastName = "Mueller", gender = "Female", rating = 7 }
    , { firstName = "Rachel", lastName = "Ng", gender = "Female", rating = 3 }
    , { firstName = "Justine", lastName = "Price", gender = "Female", rating = 7 }
    , { firstName = "Andrea", lastName = "Proulx", gender = "Female", rating = 8 }
    , { firstName = "Jessie", lastName = "Robinson", gender = "Female", rating = 8 }
    , { firstName = "Neena", lastName = "Sidhu", gender = "Female", rating = 6 }
    , { firstName = "Susan", lastName = "Sunde", gender = "Female", rating = 7 }
    , { firstName = "An", lastName = "Tran", gender = "Female", rating = 7 }
    , { firstName = "Stephanie", lastName = "Verbit", gender = "Female", rating = 6 }
    , { firstName = "Heather", lastName = "Wallace", gender = "Female", rating = 6 }
    , { firstName = "Michelle", lastName = "Warren", gender = "Female", rating = 9 }
    , { firstName = "Carrie-Anne", lastName = "Whyte", gender = "Female", rating = 6 }
    , { firstName = "Stacey", lastName = "Wowchuk", gender = "Female", rating = 6 }
    , { firstName = "Alisha", lastName = "Zhao", gender = "Female", rating = 10 }
    ]
