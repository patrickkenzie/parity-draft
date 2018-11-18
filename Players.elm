module Players exposing (..)

import Csv exposing (..)
import Csv.Decode exposing (..)
import Json.Encode as E exposing (..)
import Json.Decode as D exposing (..)


type alias Player =
    { firstName : String
    , lastName : String
    , gender : String
    , height: Int
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


compareByAsc : (Player -> comparable) -> Player -> Player -> Order
compareByAsc sort x y =
    compare (sort x) (sort y)


compareByDesc : (Player -> comparable) -> Player -> Player -> Order
compareByDesc sort x y =
    compare (sort y) (sort x)


decodePlayer : D.Decoder Player
decodePlayer =
    D.map5 Player
        (D.field "firstName" D.string)
        (D.field "lastName" D.string)
        (D.field "gender" D.string)
        (D.field "height" D.int)
        (D.field "rating" D.int)



encodePlayer : Player -> E.Value
encodePlayer player =
    E.object
        [ ( "firstName", E.string player.firstName )
        , ( "lastName", E.string player.lastName )
        , ( "gender", E.string player.gender )
        , ( "height", E.int player.height )
        , ( "rating", E.int player.rating )
        ]


fullPlayerList : List Player
fullPlayerList =
    let
        parsedPlayers =
            allPlayersParsed

        buildPlayers =
            buildPlaceholderPlayers (10 * 6) parsedPlayers
    in
        (parsedPlayers ++ buildPlayers "Female" ++ buildPlayers "Male")
            |> List.sortWith (compareByAsc .lastName)
            |> List.sortWith (compareByDesc .rating)
            |> List.sortWith (compareByAsc .gender)


isGender : String -> Player -> Bool
isGender gender player =
    player.gender == gender


buildPlaceholderPlayers : Int -> List Player -> String -> List Player
buildPlaceholderPlayers targetCount parsedPlayers gender =
    let
        createDummy gender number =
            { firstName = gender ++ " Placeholder"
            , lastName = "_" ++ (toString number)
            , gender = gender
            , height = 0
            , rating = 0
            }

        playerCount =
            List.length (List.filter (isGender gender) parsedPlayers)
    in
        -- 1-index the range to force an empty list (and nicer display!)
        List.map (createDummy gender) (List.range 1 (targetCount - playerCount))


playerDecoder : Csv.Decode.Decoder (Player -> a) a
playerDecoder =
    Csv.Decode.map Player
        (next Result.Ok
            |> andMap (next Result.Ok)
            |> andMap (next Result.Ok)
            |> andMap (next String.toInt)
            |> andMap (next String.toInt)
        )


allPlayersParsed : List Player
allPlayersParsed =
    allPlayersRaw
        |> Csv.parse
        |> Csv.Decode.decodeCsv playerDecoder
        |> Result.withDefault []


allPlayersRaw : String
allPlayersRaw =
    """
First Name,Last Name,Gender,Height,Skill Level
Vivianne,Fortin,Female,67,10
Liza,Shelley,Female,67,9
Michelle,Warren,Female,65,9
Josee,Guibord,Female,64,9
Jaime,Boss,Female,66,8
Myriam,Hebabi,Female,61,8
Katie,Wood,Female,63,8
Kristie,Ellis,Female,66,8
Melany,Bouchard,Female,70,8
Rachel,Robichaud,Female,65,8
Kate,Cavallaro,Female,67,8
Ashlin,Kelly,Female,67,8
Andrea,Proulx,Female,62,8
Jessie,Robinson,Female,65,8
Emily,Kavanagh,Female,71,7
Darlene,Riley,Female,60,7
Angela,Mueller,Female,69,7
Melissa,Jess,Female,64,7
Karen,Kavanagh,Female,64,7
Renė,Gauvin,Female,69,7
Genevieve,Labelle,Female,70,7
Justine,Price,Female,63,7
Andrea,Dietz,Female,67,7
Laura,Chambers Storey,Female,68,7
Kama,Szereszewski,Female,63,6
Eloise,Clement,Female,66,6
Adrienne,Junek,Female,63,6
Kristyn,Berquist,Female,64,6
Caleigh,Irwin,Female,64,6
Karin,Phillips,Female,67,6
Kate,Achtell,Female,61,6
Neena,Sidhu,Female,63,6
Stacey,Wowchuk,Female,62,6
Natalie,Mullin,Female,68,6
Heather,Wallace,Female,64,6
Elsbeth,Vaino,Female,69,5
Rachel,Hurdle,Female,79,5
Audrey,Nuk,Female,68,5
Erin,Courtney,Female,64,5
Wanda,Jonsson,Female,63,5
Elisa,Mantil,Female,69,5
Christine,Beals,Female,62,5
Kindha,Gorman,Female,63,5
Jennifer,Saxe,Female,61,5
Rachel,Lefebvre,Female,70,5
Marie-Ange,Gravel,Female,66,5
Alix,Ranger,Female,67,5
Hope,Celani,Female,62,5
Andrea,Brabant,Female,63,5
Clare,Gee,Female,67,5
Celine,Dumais,Female,67,4
Julia,Riddick,Female,70,4
Katherine,Schaefer,Female,66,4
AnneMarie,Gagnon,Female,65,3
Emily,MacDonald,Female,63,3
Rachel,Ng,Female,60,3
Annie,Christie,Female,69,3
Linh,Pham-Vo,Female,63,3
Patrick,Mapp,Male,73,10
Matthew,Cole,Male,70,10
Martin,Cloake,Male,79,9
Ryan,Wallace,Male,73,9
Marcus,Bordage,Male,72,8
Adam,MacDonald,Male,66,8
Brian,O'Callaghan,Male,70,8
Pascal,Michaud,Male,69,8
Craig,Anderson,Male,67,8
Chris,Sullivan,Male,72,8
Geofford,Seaborn,Male,75,8
Travis,Davidson,Male,72,8
Tom,Newman,Male,74,8
Christopher,Keates,Male,75,8
Mark,Donahue,Male,72,8
Hadrian,Mertins-Kirkwood,Male,73,8
Brian,Perry,Male,72,8
Luca,Lafontaine,Male,68,7
Douglas,Brierley,Male,70,7
Jamie,Wildgen,Male,72,7
Shubho Bo,Biswas,Male,66,7
Stephen,Close,Male,71,7
Alessandro,Colantonio,Male,70,7
Nick,Amlin,Male,72,7
Greg,Probe,Male,70,7
Michael,O'Hare,Male,72,7
Thomas,Ferguson,Male,74,7
Mehmet,Karman,Male,72,7
John,Haig,Male,71,7
Steve,Bisang,Male,69,7
Nicholas,Aghajanian,Male,68,7
Kelsey,Charie,Male,67,7
Dan,Thomson,Male,70,7
Nick,Theriault,Male,72,7
Jared,Cohen,Male,67,7
Christopher,Castonguay,Male,71,7
Trevor,Stocki,Male,36,6
Calvin,Wiebe,Male,67,6
Nick,Klimowicz,Male,74,6
Greg,Kung,Male,73,6
Wing-Leung,Chan,Male,67,6
Morgan,Howard,Male,73,6
Jon,Rowe,Male,73,6
Tim,Kealey,Male,72,6
Allan,Godding,Male,75,6
Rob,Tyson,Male,70,6
Giulian,De La Merced,Male,67,6
Jim,Robinson,Male,76,6
Sebastien,Belanger,Male,72,6
Patrick,Kenzie,Male,66,6
John,Siwiec,Male,69,5
Matthew,Schijns,Male,72,5
Jeff,Hunt,Male,69,5
Graham,Brown,Male,70,5
David,Townsend,Male,71,5
Van,Mardian,Male,74,5
Jonathan,Champagne,Male,67,5
Micheal,Davidson,Male,60,3
Yingdi,Wu,Male,66,3
Brian,Kells,Male,72,3
"""
