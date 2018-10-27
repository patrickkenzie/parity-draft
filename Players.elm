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
            |> andMap (next (\_ ->  Result.Ok 0))
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
First Name,Last Name,Gender,
Ewelina,Sosnowska,Female,
Wanda,Jonsson,Female,
Kama,Szereszewski,Female,
Audrey,Nuk,Female,
Erin,Courtney,Female,
Jaime,Boss,Female,
Myriam,Hebabi,Female,
Elisa,Mantil,Female,
Christine,Beals,Female,
Katie,Wood,Female,
Kindha,Gorman,Female,
Liza,Shelley,Female,
Eloise,Clement,Female,
Kristie,Ellis,Female,
Emily,MacDonald,Female,
Jennifer,Saxe,Female,
Emily,Kavanagh,Female,
Rachel,Lefebvre,Female,
Adrienne,Junek,Female,
Darlene,Riley,Female,
Rachel,Ng,Female,
Melany,Bouchard,Female,
Michelle,Warren,Female,
Celine,Dumais,Female,
Marie-Ange,Gravel,Female,
Julia,Riddick,Female,
Kristyn,Berquist,Female,
Marcus,Bordage,Male,
Rachel,Robichaud,Female,
Alix,Ranger,Female,
Caleigh,Irwin,Female,
Hope,Celani,Female,
Annie,Christie,Female,
Angela,Mueller,Female,
Patrick,Mapp,Male,
Karin,Phillips,Female,
Luca,Lafontaine,Male,
Melissa,Jess,Female,
Douglas,Brierley,Male,
Karen,Kavanagh,Female,
Adam,MacDonald,Male,
Jamie,Wildgen,Male,
John,Siwiec,Male,
Trevor,Stocki,Male,
Shubho Bo,Biswas,Male,
René,Gauvin,Female,
Calvin,Wiebe,Male,
Stephen,Close,Male,
Nick,Klimowicz,Male,
Kate,Cavallaro,Female,
Kate,Achtell,Female,
Greg,Kung,Male,
Neena,Sidhu,Female,
Alessandro,Colantonio,Male,
Martin,Cloake,Male,
Matthew,Schijns,Male,
Brian,O'Callaghan,Male,
Pascal,Michaud,Male,
Wing-Leung,Chan,Male,
Nick,Amlin,Male,
Ashlin,Kelly,Female,
Matthew,Cole,Male,
Jeff,Hunt,Male,
Stacey,Wowchuk,Female,
Greg,Probe,Male,
Michael,O'Hare,Male,
Ryan,Wallace,Male,
Thomas,Ferguson,Male,
Craig,Anderson,Male,
Morgan,Howard,Male,
Genevieve,Labelle,Female,
Graham,Brown,Male,
Jon,Rowe,Male,
Chris,Sullivan,Male,
Tim,Kealey,Male,
Mehmet,Karman,Male,
Natalie,Mullin,Female,
Allan,Godding,Male,
John,Haig,Male,
Rob,Tyson,Male,
Giulian,De La Merced,Male,
Justine,Price,Female,
Geofford,Seaborn,Male,
Josee,Guibord,Female,
Jim,Robinson,Male,
Travis,Davidson,Male,
Steve,Bisang,Male,
Andrea,Dietz,Female,
David,Townsend,Male,
Nicholas,Aghajanian,Male,
Tom,Newman,Male,
Christopher,Keates,Male,
Kelsey,Charie,Male,
Micheal,Davidson,Male,
Laura,Chambers Storey,Female,
Mark,Donahue,Male,
Yingdi,Wu,Male,
Andrea,Brabant,Female,
Van,Mardian,Male,
Jonathan,Champagne,Male,
Dan,Thomson,Male,
Andrea,Proulx,Female,
Heather,Wallace,Female,
Nick,Theriault,Male,
Clare,Gee,Female,
Hadrian,Mertins-Kirkwood,Male,
Sebastien,Belanger,Male,
Jessie,Robinson,Female,
Brian,Perry,Male,
Jared,Cohen,Male,
Christopher,Castonguay,Male,
Patrick,Kenzie,Male,
Brian,Kells,Male,
"""
