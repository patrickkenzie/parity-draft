module Players exposing (..)


type Gender
    = Female
    | Male


type alias Player =
    { name : String
    , gender : Gender
    , rating : Int
    , salary : Int
    }


sortPlayers : List Player -> List Player
sortPlayers players =
    List.sortBy .salary allPlayers |> List.reverse


players : List Player
players =
    sortPlayers allPlayers


allPlayers : List Player
allPlayers =
    [ { name = "Nicholas Aghajanian"
      , gender = Male
      , rating = 7
      , salary = 1400000
      }
    , { name = "Craig Anderson"
      , gender = Male
      , rating = 8
      , salary = 1749000
      }
    , { name = "Kevin Barford"
      , gender = Male
      , rating = 6
      , salary = 1495928
      }
    , { name = "Sebastien Belanger"
      , gender = Male
      , rating = 6
      , salary = 1157000
      }
    , { name = "Simon Berry"
      , gender = Male
      , rating = 5
      , salary = 1015100
      }
    , { name = "Steve Bisang"
      , gender = Male
      , rating = 7
      , salary = 1167428
      }
    , { name = "Marcus Bordage"
      , gender = Male
      , rating = 8
      , salary = 998497
      }
    , { name = "Graham Brown"
      , gender = Male
      , rating = 5
      , salary = 1071000
      }
    , { name = "Jeremy Bryan"
      , gender = Male
      , rating = 7
      , salary = 1270500
      }
    , { name = "Brent Burton"
      , gender = Male
      , rating = 6
      , salary = 1493500
      }
    , { name = "Frederic Caron"
      , gender = Male
      , rating = 9
      , salary = 1371928
      }
    , { name = "Jonathan Champagne"
      , gender = Male
      , rating = 4
      , salary = 940132
      }
    , { name = "Wing-Leung Chan"
      , gender = Male
      , rating = 6
      , salary = 998497
      }
    , { name = "Steve Chow"
      , gender = Male
      , rating = 6
      , salary = 1831500
      }
    , { name = "Martin Cloake"
      , gender = Male
      , rating = 9
      , salary = 1537900
      }
    , { name = "Stephen Close"
      , gender = Male
      , rating = 7
      , salary = 998497
      }
    , { name = "Michael Colantonio"
      , gender = Male
      , rating = 6
      , salary = 1307500
      }
    , { name = "Alessandro Colantonio"
      , gender = Male
      , rating = 7
      , salary = 1539500
      }
    , { name = "Sina Dee"
      , gender = Male
      , rating = 7
      , salary = 1922611
      }
    , { name = "Mark Donahue"
      , gender = Male
      , rating = 8
      , salary = 1205214
      }
    , { name = "Jason Fraser"
      , gender = Male
      , rating = 6
      , salary = 1094600
      }
    , { name = "Richard Gregory"
      , gender = Male
      , rating = 6
      , salary = 1256650
      }
    , { name = "Scott Higgins"
      , gender = Male
      , rating = 7
      , salary = 1857388
      }
    , { name = "Morgan Howard"
      , gender = Male
      , rating = 6
      , salary = 1414333
      }
    , { name = "Kevin Hughes"
      , gender = Male
      , rating = 7
      , salary = 1449166
      }
    , { name = "Jeff Hunt"
      , gender = Male
      , rating = 6
      , salary = 1190500
      }
    , { name = "Rob Ives"
      , gender = Male
      , rating = 7
      , salary = 1241000
      }
    , { name = "Tim Kealey"
      , gender = Male
      , rating = 6
      , salary = 1083276
      }
    , { name = "Christopher Keates"
      , gender = Male
      , rating = 8
      , salary = 1893000
      }
    , { name = "Brian Kells"
      , gender = Male
      , rating = 8
      , salary = 1537500
      }
    , { name = "Patrick Kenzie"
      , gender = Male
      , rating = 6
      , salary = 1358500
      }
    , { name = "Sonny Kim"
      , gender = Male
      , rating = 4
      , salary = 798798
      }
    , { name = "Nick Klimowicz"
      , gender = Male
      , rating = 6
      , salary = 1337392
      }
    , { name = "Krys Kudakiewicz"
      , gender = Male
      , rating = 6
      , salary = 1495356
      }
    , { name = "Will Leckie"
      , gender = Male
      , rating = 8
      , salary = 1069000
      }
    , { name = "Amos Lee"
      , gender = Male
      , rating = 8
      , salary = 1653500
      }
    , { name = "Mike Lee"
      , gender = Male
      , rating = 10
      , salary = 2034704
      }
    , { name = "Owen Lumley"
      , gender = Male
      , rating = 8
      , salary = 2148000
      }
    , { name = "Patrick McKelvey"
      , gender = Male
      , rating = 6
      , salary = 998497
      }
    , { name = "Hadrian Mertins-Kirkwood"
      , gender = Male
      , rating = 8
      , salary = 1849900
      }
    , { name = "Tyler Mulcock"
      , gender = Male
      , rating = 6
      , salary = 1475000
      }
    , { name = "Ryan Mussell"
      , gender = Male
      , rating = 5
      , salary = 1186341
      }
    , { name = "Tom Newman"
      , gender = Male
      , rating = 9
      , salary = 1397896
      }
    , { name = "Michael O'Hare"
      , gender = Male
      , rating = 6
      , salary = 998497
      }
    , { name = "Jonathan Pindur"
      , gender = Male
      , rating = 7
      , salary = 1467636
      }
    , { name = "Benjamin Piper"
      , gender = Male
      , rating = 7
      , salary = 1838221
      }
    , { name = "Greg Probe"
      , gender = Male
      , rating = 7
      , salary = 1135249
      }
    , { name = "Jim Robinson"
      , gender = Male
      , rating = 6
      , salary = 1433500
      }
    , { name = "Peter Roebuck"
      , gender = Male
      , rating = 6
      , salary = 1110530
      }
    , { name = "Jon Rowe"
      , gender = Male
      , rating = 6
      , salary = 1597500
      }
    , { name = "Trevor Ryan"
      , gender = Male
      , rating = 6
      , salary = 1759500
      }
    , { name = "Matthew Schijns"
      , gender = Male
      , rating = 5
      , salary = 1286611
      }
    , { name = "Andre Scott"
      , gender = Male
      , rating = 5
      , salary = 1110000
      }
    , { name = "Geofford seaborn"
      , gender = Male
      , rating = 8
      , salary = 1593500
      }
    , { name = "Andrew Spearin"
      , gender = Male
      , rating = 6
      , salary = 1644000
      }
    , { name = "Kyle Sprysa"
      , gender = Male
      , rating = 9
      , salary = 939000
      }
    , { name = "Trevor Stocki"
      , gender = Male
      , rating = 6
      , salary = 623333
      }
    , { name = "Chris Sullivan"
      , gender = Male
      , rating = 8
      , salary = 1919500
      }
    , { name = "David Townsend"
      , gender = Male
      , rating = 5
      , salary = 1320687
      }
    , { name = "Chris Tran"
      , gender = Male
      , rating = 6
      , salary = 1153200
      }
    , { name = "Jay Thor Turner"
      , gender = Male
      , rating = 6
      , salary = 1160000
      }
    , { name = "Rob Tyson"
      , gender = Male
      , rating = 6
      , salary = 1098500
      }
    , { name = "Jamie Wildgen"
      , gender = Male
      , rating = 7
      , salary = 1684443
      }
    , { name = "Edwin Wong"
      , gender = Male
      , rating = 5
      , salary = 798798
      }
    , { name = "Christine Beals"
      , gender = Female
      , rating = 5
      , salary = 776000
      }
    , { name = "Jaime Boss"
      , gender = Female
      , rating = 9
      , salary = 1122000
      }
    , { name = "Laura Chambers Storey"
      , gender = Female
      , rating = 7
      , salary = 1267213
      }
    , { name = "Meagan Doyle"
      , gender = Female
      , rating = 8
      , salary = 998497
      }
    , { name = "Wynne Gee"
      , gender = Female
      , rating = 9
      , salary = 1305339
      }
    , { name = "Kindha Gorman"
      , gender = Female
      , rating = 6
      , salary = 937500
      }
    , { name = "Josee Guibord"
      , gender = Female
      , rating = 9
      , salary = 1397896
      }
    , { name = "Sandra Hanson"
      , gender = Female
      , rating = 7
      , salary = 857499
      }
    , { name = "Melissa Jess"
      , gender = Female
      , rating = 6
      , salary = 1110856
      }
    , { name = "Karen Kavanagh"
      , gender = Female
      , rating = 6
      , salary = 998497
      }
    , { name = "Ashlin Kelly"
      , gender = Female
      , rating = 7
      , salary = 1815833
      }
    , { name = "Laura Knowles"
      , gender = Female
      , rating = 6
      , salary = 1319475
      }
    , { name = "Julia Laforge"
      , gender = Female
      , rating = 8
      , salary = 1098200
      }
    , { name = "Samantha Lee"
      , gender = Female
      , rating = 9
      , salary = 1090500
      }
    , { name = "Heather McCabe"
      , gender = Female
      , rating = 9
      , salary = 1645070
      }
    , { name = "Angela Mueller"
      , gender = Female
      , rating = 7
      , salary = 1229000
      }
    , { name = "Aleksandra Ostojic"
      , gender = Female
      , rating = 7
      , salary = 1360000
      }
    , { name = "Maya Popovic"
      , gender = Female
      , rating = 7
      , salary = 1415000
      }
    , { name = "Justine Price"
      , gender = Female
      , rating = 7
      , salary = 1440000
      }
    , { name = "Andrea Proulx"
      , gender = Female
      , rating = 8
      , salary = 1348888
      }
    , { name = "Kirsten Querbach"
      , gender = Female
      , rating = 7
      , salary = 1501238
      }
    , { name = "Nina Ramic"
      , gender = Female
      , rating = 5
      , salary = 1205917
      }
    , { name = "Darlene Riley"
      , gender = Female
      , rating = 7
      , salary = 1285185
      }
    , { name = "Megan Robb"
      , gender = Female
      , rating = 6
      , salary = 1587900
      }
    , { name = "Jessie Robinson"
      , gender = Female
      , rating = 8
      , salary = 1303454
      }
    , { name = "An Tran"
      , gender = Female
      , rating = 7
      , salary = 1095500
      }
    , { name = "Carrie-Anne Whyte"
      , gender = Female
      , rating = 5
      , salary = 1066400
      }
    , { name = "Carissa Wong"
      , gender = Female
      , rating = 7
      , salary = 998497
      }
    , { name = "Stacey Wowchuk"
      , gender = Female
      , rating = 6
      , salary = 1033000
      }
    , { name = "Taka Yamada"
      , gender = Female
      , rating = 5
      , salary = 1034350
      }
    , { name = "Rachel Young"
      , gender = Female
      , rating = 8
      , salary = 998497
      }
    , { name = "Alisha Zhao"
      , gender = Female
      , rating = 10
      , salary = 1397896
      }
    ]
