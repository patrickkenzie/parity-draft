module Players exposing (..)

import Csv exposing (..)
import Csv.Decode exposing (..)


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


playerDecoder : Csv.Decode.Decoder (Player -> a) a
playerDecoder =
    map Player
        (next Result.Ok
            |> andMap (next Result.Ok)
            |> andMap (next Result.Ok)
            |> andMap (next String.toInt)
        )


allPlayers : List Player
allPlayers =
    allPlayersRaw
        |> Csv.parse
        |> Csv.Decode.decodeCsv playerDecoder
        |> Result.withDefault []


allPlayersRaw : String
allPlayersRaw =
    """
First Name,Last Name,Gender,Skill Level
Nicholas,Aghajanian,Male,7
Nick,Amlin,Male,6
Craig,Anderson,Male,8
Kevin,Barford,Male,6
Sebastien,Belanger,Male,6
Simon,Berry,Male,5
Shubho Bo,Biswas,Male,7
Lance,Blackstock,Male,10
Ryan,Briggs,Male,8
Graham,Brown,Male,5
Frederic,Caron,Male,9
Christopher,Castonguay,Male,7
Jonathan,Champagne,Male,5
Wing-Leung,Chan,Male,6
Marc,Charette,Male,4
Kelsey,Charie,Male,7
Martin,Cloake,Male,9
Stephen,Close,Male,7
Michael,Colantonio,Male,6
Alessandro,Colantonio,Male,7
Ben,Curran,Male,5
Micheal,Davidson,Male,3
Jason,Fraser,Male,6
Allan,Godding,Male,6
Richard,Gregory,Male,6
John,Haig,Male,7
Scott,Higgins,Male,7
Derek,Hodgson,Male,8
Craig,Holden,Male,6
Jeff,Hunt,Male,6
Rob,Ives,Male,7
Mehmet,Karman,Male,7
Tim,Kealey,Male,6
Christopher,Keates,Male,8
Brian,Kells,Male,8
Patrick,Kenzie,Male,6
Nick,Klimowicz,Male,6
Mike,Lee,Male,10
Ken,Maclean,Male,8
Hadrian,Mertins-Kirkwood,Male,8
Tom,Newman,Male,8
David,O'Connor,Male,6
Brian,Perry,Male,7
Jonathan,Pindur,Male,7
Benjamin,Piper,Male,7
Greg,Probe,Male,7
Will,Reid,Male,6
Jim,Robinson,Male,6
Jon,Rowe,Male,6
Thomas,Sattolo,Male,5
Matthew,Schijns,Male,5
Andre,Scott,Male,5
Geofford,Seaborn,Male,8
Andrew,Spearin,Male,6
Trevor,Stocki,Male,6
Chris,Sullivan,Male,8
Nick,Theriault,Male,6
Dan,Thomson,Male,7
David,Townsend,Male,5
Jay Thor,Turner,Male,6
Rob,Tyson,Male,6
Jamie,Wildgen,Male,7
Edwin,Wong,Male,5
Michael,Wong,Male,5
Kate,Achtell,Female,6
Christine,Beals,Female,5
Kristyn,Berquist,Female,2
Cassie,Berquist,Female,9
Jaime,Boss,Female,8
Melany,Bouchard,Female,8
Hope,Celani,Female,5
Laura,Chambers Storey,Female,7
Celine,Dumais,Female,4
Kristie,Ellis,Female,8
Tanya,Gallant,Female,5
Clare,Gee,Female,5
Marie-Ange,Gravel,Female,5
Josee,Guibord,Female,9
Melissa,Jess,Female,6
Adrienne,Junek,Female,6
Ashlin,Kelly,Female,7
Vanessa,Mann,Female,8
Angela,Mueller,Female,7
Rachel,Ng,Female,3
Justine,Price,Female,7
Andrea,Proulx,Female,8
Jessie,Robinson,Female,8
Neena,Sidhu,Female,6
Susan,Sunde,Female,7
An,Tran,Female,7
Stephanie,Verbit,Female,6
Heather,Wallace,Female,6
Michelle,Warren,Female,9
Carrie-Anne,Whyte,Female,6
Stacey,Wowchuk,Female,6
Alisha,Zhao,Female,10
"""
