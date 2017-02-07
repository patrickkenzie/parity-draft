module Players exposing (..)

import Data exposing (..)

players : List Player
players = List.sortBy .salary allPlayers |> List.reverse

allPlayers : List Player
allPlayers = [
    {
      name = "Nicholas Aghajanian",
      gender = Male,
      rating = 7,
      salary = 1353000
    },
    {
      name = "Craig Anderson",
      gender = Male,
      rating = 8,
      salary = 1608500
    },
    {
      name = "Kevin Barford",
      gender = Male,
      rating = 6,
      salary = 1451428
    },
    {
      name = "Sebastien Belanger",
      gender = Male,
      rating = 6,
      salary = 1093500
    },
    {
      name = "Simon Berry",
      gender = Male,
      rating = 5,
      salary = 983100
    },
    {
      name = "Steve Bisang",
      gender = Male,
      rating = 7,
      salary = 1098428
    },
    {
      name = "Marcus Bordage",
      gender = Male,
      rating = 8,
      salary = 939936
    },
    {
      name = "Jeremy Bryan",
      gender = Male,
      rating = 7,
      salary = 1232500
    },
    {
      name = "Brent Burton",
      gender = Male,
      rating = 6,
      salary = 1422000
    },
    {
      name = "Frederic Caron",
      gender = Male,
      rating = 9,
      salary = 1283928
    },
    {
      name = "Jonathan Champagne",
      gender = Male,
      rating = 4,
      salary = 879132
    },
    {
      name = "Steve Chow",
      gender = Male,
      rating = 6,
      salary = 1686000
    },
    {
      name = "Martin Cloake",
      gender = Male,
      rating = 9,
      salary = 1417900
    },
    {
      name = "Michael Colantonio",
      gender = Male,
      rating = 6,
      salary = 1242500
    },
    {
      name = "Alessandro Colantonio",
      gender = Male,
      rating = 7,
      salary = 1454500
    },
    {
      name = "Sina Dee",
      gender = Male,
      rating = 7,
      salary = 1782611
    },
    {
      name = "Mark Donahue",
      gender = Male,
      rating = 8,
      salary = 1116214
    },
    {
      name = "Jason Fraser",
      gender = Male,
      rating = 6,
      salary = 1045050
    },
    {
      name = "Richard Gregory",
      gender = Male,
      rating = 6,
      salary = 1200150
    },
    {
      name = "Scott Higgins",
      gender = Male,
      rating = 7,
      salary = 1734388
    },
    {
      name = "Morgan Howard",
      gender = Male,
      rating = 6,
      salary = 1302333
    },
    {
      name = "Kevin Hughes",
      gender = Male,
      rating = 7,
      salary = 1365166
    },
    {
      name = "Jeff Hunt",
      gender = Male,
      rating = 6,
      salary = 1131500
    },
    {
      name = "Rob Ives",
      gender = Male,
      rating = 7,
      salary = 1238000
    },
    {
      name = "Tim Kealey",
      gender = Male,
      rating = 6,
      salary = 1036388
    },
    {
      name = "Christopher Keates",
      gender = Male,
      rating = 8,
      salary = 1758000
    },
    {
      name = "Brian Kells",
      gender = Male,
      rating = 8,
      salary = 1435500
    },
    {
      name = "Stanley Kent",
      gender = Male,
      rating = 6,
      salary = 1408571
    },
    {
      name = "Patrick Kenzie",
      gender = Male,
      rating = 6,
      salary = 1257500
    },
    {
      name = "Sonny Kim",
      gender = Male,
      rating = 4,
      salary = 751949
    },
    {
      name = "Nick Klimowicz",
      gender = Male,
      rating = 6,
      salary = 1324892
    },
    {
      name = "Krys Kudakiewicz",
      gender = Male,
      rating = 6,
      salary = 1367856
    },
    {
      name = "Will Leckie",
      gender = Male,
      rating = 8,
      salary = 1034000
    },
    {
      name = "Amos Lee",
      gender = Male,
      rating = 8,
      salary = 1514500
    },
    {
      name = "Mike Lee",
      gender = Male,
      rating = 10,
      salary = 1943704
    },
    {
      name = "Owen Lumley",
      gender = Male,
      rating = 8,
      salary = 1966000
    },
    {
      name = "Hadrian Mertins-Kirkwood",
      gender = Male,
      rating = 8,
      salary = 1741400
    },
    {
      name = "Tyler Mulcock",
      gender = Male,
      rating = 6,
      salary = 1372000
    },
    {
      name = "Ryan Mussell",
      gender = Male,
      rating = 5,
      salary = 1125699
    },
    {
      name = "Tom Newman",
      gender = Male,
      rating = 9,
      salary = 1315910
    },
    {
      name = "Michael O'Hare",
      gender = Male,
      rating = 6,
      salary = 939936
    },
    {
      name = "Jonathan Pindur",
      gender = Male,
      rating = 7,
      salary = 1387000
    },
    {
      name = "Benjamin Piper",
      gender = Male,
      rating = 7,
      salary = 1666221
    },
    {
      name = "Greg Probe",
      gender = Male,
      rating = 7,
      salary = 1081687
    },
    {
      name = "Jim Robinson",
      gender = Male,
      rating = 6,
      salary = 1383500
    },
    {
      name = "Peter Roebuck",
      gender = Male,
      rating = 6,
      salary = 1061086
    },
    {
      name = "Jon Rowe",
      gender = Male,
      rating = 6,
      salary = 1473500
    },
    {
      name = "Trevor Ryan",
      gender = Male,
      rating = 6,
      salary = 1619000
    },
    {
      name = "Matthew Schijns",
      gender = Male,
      rating = 5,
      salary = 1232611
    },
    {
      name = "Andre Scott",
      gender = Male,
      rating = 5,
      salary = 1077000
    },
    {
      name = "Geofford seaborn",
      gender = Male,
      rating = 8,
      salary = 1462000
    },
    {
      name = "Andrew Spearin",
      gender = Male,
      rating = 6,
      salary = 1527000
    },
    {
      name = "Kyle Sprysa",
      gender = Male,
      rating = 9,
      salary = 909000
    },
    {
      name = "Trevor Stocki",
      gender = Male,
      rating = 6,
      salary = 605333
    },
    {
      name = "Chris Sullivan",
      gender = Male,
      rating = 8,
      salary = 1811000
    },
    {
      name = "Dan Thomson",
      gender = Male,
      rating = 7,
      salary = 1254284
    },
    {
      name = "David Townsend",
      gender = Male,
      rating = 5,
      salary = 1240187
    },
    {
      name = "Chris Tran",
      gender = Male,
      rating = 6,
      salary = 1061700
    },
    {
      name = "Jay Thor Turner",
      gender = Male,
      rating = 6,
      salary = 1077500
    },
    {
      name = "Rob Tyson",
      gender = Male,
      rating = 6,
      salary = 1054500
    },
    {
      name = "Jamie Wildgen",
      gender = Male,
      rating = 7,
      salary = 1580443
    },
    {
      name = "Edwin Wong",
      gender = Male,
      rating = 5,
      salary = 751949
    },
    {
      name = "Christine Beals",
      gender = Female,
      rating = 5,
      salary = 755500
    },
    {
      name = "Jaime Boss",
      gender = Female,
      rating = 9,
      salary = 1071000
    },
    {
      name = "Laura Chambers Storey",
      gender = Female,
      rating = 7,
      salary = 1189213
    },
    {
      name = "Meagan Doyle",
      gender = Female,
      rating = 8,
      salary = 939936
    },
    {
      name = "Wynne Gee",
      gender = Female,
      rating = 9,
      salary = 1184339
    },
    {
      name = "Kindha Gorman",
      gender = Female,
      rating = 6,
      salary = 879000
    },
    {
      name = "Sandra Hanson",
      gender = Female,
      rating = 7,
      salary = 837499
    },
    {
      name = "Melissa Jess",
      gender = Female,
      rating = 6,
      salary = 1045856
    },
    {
      name = "Karen Kavanagh",
      gender = Female,
      rating = 6,
      salary = 939936
    },
    {
      name = "Ashlin Kelly",
      gender = Female,
      rating = 7,
      salary = 1726833
    },
    {
      name = "Laura Knowles",
      gender = Female,
      rating = 6,
      salary = 1283475
    },
    {
      name = "Julia Laforge",
      gender = Female,
      rating = 8,
      salary = 1048350
    },
    {
      name = "Samantha Lee",
      gender = Female,
      rating = 9,
      salary = 1038500
    },
    {
      name = "Heather McCabe",
      gender = Female,
      rating = 9,
      salary = 1586570
    },
    {
      name = "Angela Mueller",
      gender = Female,
      rating = 7,
      salary = 1161000
    },
    {
      name = "Aleksandra Ostojic",
      gender = Female,
      rating = 7,
      salary = 1285000
    },
    {
      name = "Maya Popovic",
      gender = Female,
      rating = 7,
      salary = 1327000
    },
    {
      name = "Justine Price",
      gender = Female,
      rating = 7,
      salary = 1347000
    },
    {
      name = "Andrea Proulx",
      gender = Female,
      rating = 8,
      salary = 1265888
    },
    {
      name = "Kirsten Querbach",
      gender = Female,
      rating = 7,
      salary = 1417238
    },
    {
      name = "Nina Ramic",
      gender = Female,
      rating = 5,
      salary = 1149542
    },
    {
      name = "Darlene Riley",
      gender = Female,
      rating = 7,
      salary = 1213185
    },
    {
      name = "Megan Robb",
      gender = Female,
      rating = 6,
      salary = 1488400
    },
    {
      name = "Jessie Robinson",
      gender = Female,
      rating = 8,
      salary = 1236500
    },
    {
      name = "An Tran",
      gender = Female,
      rating = 7,
      salary = 1041500
    },
    {
      name = "Carrie-Anne Whyte",
      gender = Female,
      rating = 5,
      salary = 982900
    },
    {
      name = "Carissa Wong",
      gender = Female,
      rating = 7,
      salary = 939936
    },
    {
      name = "Stacey Wowchuk",
      gender = Female,
      rating = 6,
      salary = 984000
    },
    {
      name = "Taka Yamada",
      gender = Female,
      rating = 5,
      salary = 1023350
    },
    {
      name = "Rachel Young",
      gender = Female,
      rating = 8,
      salary = 939936
    },
    {
      name = "Alisha Zhao",
      gender = Female,
      rating = 10,
      salary = 1315910
    }
  ]
