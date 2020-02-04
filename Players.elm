module Players exposing (..)

import Csv exposing (..)
import Csv.Decode exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)


type alias Player =
    { id : PlayerID
    , firstName : String
    , lastName : String
    , gender : Gender
    , height : Int
    , rating : Int
    , salary : Float
    , hmk : Float
    }


type alias PlayerID =
    Int


type Gender
    = Female
    | Male


genderDecoder : D.Decoder Gender
genderDecoder =
    D.string |> D.andThen (fromResult << parseGender)


fromResult : Result String a -> D.Decoder a
fromResult result =
    case result of
        Ok a ->
            D.succeed a

        Err err ->
            D.fail err


parseGender : String -> Result String Gender
parseGender gender =
    case gender of
        "female" ->
            Ok Female

        "Female" ->
            Ok Female

        "woman" ->
            Ok Female

        "Woman" ->
            Ok Female

        "male" ->
            Ok Male

        "Male" ->
            Ok Male

        "open" ->
            Ok Male

        "Open" ->
            Ok Male

        _ ->
            Err ("Invalid Gender: " ++ gender)


genderToString : Gender -> String
genderToString gender =
    case gender of
        Female ->
            "female"

        Male ->
            "male"


className : Player -> String
className player =
    genderToString player.gender


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
    D.map8 Player
        (D.field "id" D.int)
        (D.field "firstName" D.string)
        (D.field "lastName" D.string)
        (D.field "gender" genderDecoder)
        (D.field "height" D.int)
        (D.field "rating" D.int)
        (D.field "salary" D.float)
        (D.field "hmk" D.float)


encodePlayer : Player -> E.Value
encodePlayer player =
    E.object
        [ ( "id", E.int player.id )
        , ( "firstName", E.string player.firstName )
        , ( "lastName", E.string player.lastName )
        , ( "gender", E.string (genderToString player.gender) )
        , ( "height", E.int player.height )
        , ( "rating", E.int player.rating )
        , ( "salary", E.float player.salary )
        , ( "hmk", E.float player.hmk )
        ]


buildPlayerList : Int -> List Player -> List Player
buildPlayerList teamCount players =
    let
        buildPlayers =
            buildPlaceholderPlayers teamCount players
    in
    (players ++ buildPlayers Female ++ buildPlayers Male)
        |> List.sortWith (compareByAsc .lastName)
        |> List.sortWith (compareByDesc .rating)


defaultPlayerList : Int -> List Player
defaultPlayerList teamCount =
    buildPlayerList teamCount allPlayersParsed


isGender : Gender -> Player -> Bool
isGender gender player =
    player.gender == gender


buildPlaceholderPlayers : Int -> List Player -> Gender -> List Player
buildPlaceholderPlayers teamCount parsedPlayers gender =
    let
        createDummy number =
            { id = playerCount + number
            , firstName = genderToString gender ++ " Placeholder"
            , lastName = "_" ++ String.padLeft 2 '0' (String.fromInt number)
            , gender = gender
            , height = 0
            , rating = 0
            , salary = 0
            , hmk = 0
            }

        playerCount =
            List.length (List.filter (isGender gender) parsedPlayers)

        rosterCount =
            case gender of
                Female ->
                    4

                Male ->
                    8

        targetCount =
            (teamCount * rosterCount) - playerCount
    in
    -- 1-index the range to force an empty list (and nicer display!)
    List.map createDummy (List.range 1 targetCount)


parseInt : String -> Result String Int
parseInt value =
    case String.toInt value of
        Just int ->
            Result.Ok int

        Nothing ->
            Result.Err ("Invalid int: " ++ value)


parseFloat : String -> Result String Float
parseFloat value =
    case String.toFloat value of
        Just float ->
            Result.Ok float

        Nothing ->
            Result.Ok 0


playerDecoder : Csv.Decode.Decoder (Player -> a) a
playerDecoder =
    Csv.Decode.map Player
        (next parseInt
            |> andMap (next Result.Ok)
            |> andMap (next Result.Ok)
            |> andMap (next parseGender)
            |> andMap (next parseInt)
            |> andMap (next parseInt)
            |> andMap (next parseFloat)
            |> andMap (next parseFloat)
        )


addIds : Csv.Csv -> Csv.Csv
addIds csv =
    let
        mapper int value =
            String.fromInt int :: value

        idRecords =
            List.indexedMap mapper csv.records
    in
    { csv
        | headers = "Id" :: csv.headers
        , records = idRecords
    }


allPlayersParsed : List Player
allPlayersParsed =
    allPlayersRaw
        |> Csv.parse
        |> addIds
        |> Csv.Decode.decodeCsv playerDecoder
        |> Result.withDefault []


allPlayersRaw : String
allPlayersRaw =
    """
First Name,Last Name,Gender,Height,Skill,Salary,HMK
Ainsley,Shannon,Woman,68,5,,
Alicia,Kehoe,Woman,68,5,,
Alisha,Zhao,Woman,64,10,5275,5.9
Alyssa,Mainwood,Woman,68,5,3588,4.2
Andrea,Dietz,Woman,67,8,2382,3.3
Angela,Mueller,Woman,69,7,2610,3.6
Caitlin,Hesketh,Woman,65,6,,
Caralynn,Laszlo,Woman,64,6,,
Celine,Dumais,Woman,67,4,2283,4.5
Charlotte,Mussells,Woman,66,6,3609,4.5
Corinne,Dunwoody,Woman,68,9,3775,5.7
Dana,Strauss,Woman,65,7,2003,3.2
Deborah,Murphy,Woman,65,6,2885,5.2
Hannah,Lewis,Woman,68,6,3364,3.7
Heather,Wallace,Woman,64,6,4471,5.3
Jessie,Robinson,Woman,66,9,4150,5.0
Josee,Guibord,Woman,64,9,4085,5.8
Julia,Riddick,Woman,70,5,2189,3.1
Justine,Dagenais,Woman,64,6,4500,5.8
Justine,Price,Woman,63,7,4170,5.6
Kama,Szereszewski,Woman,63,7,2405,3.6
Karen,Kavanagh,Woman,64,7,1628,2.5
Kate,Achtell,Woman,61,6,4112,5.9
Kate,Cavallaro,Woman,67,8,2670,4.0
Katelyn,Fontaine,Woman,68,5,3172,4.1
Kindha,Gorman,Woman,63,6,2412,3.7
Kristie,Ellis,Woman,66,8,3699,5.0
Kristyn,Berquist,Woman,64,5,1647,2.6
Laura Chambers,Storey,Woman,68,7,2532,2.6
Linh,Pham-Vo,Woman,62,3,1523,4.0
Marie-Ange,Gravel,Woman,66,6,2919,3.2
Melany,Bouchard,Woman,70,9,3686,6.4
Melissa,Jess,Woman,64,8,2282,3.4
Natalie,Mullin,Woman,68,6,2945,4.0
Neena,Sidhu,Woman,63,6,1982,2.6
Rachel,Hurdle,Woman,67,7,3060,4.7
Rachel,Ng,Woman,60,5,3641,4.6
Rachel,Young,Woman,68,9,2532,3.1
Sarah,Thompson,Woman,69,3,1661,3.6
Stephanie,Jack,Woman,67,5,2438,3.1
Abe,Greenspoon,Open,69,6,3721,5.2
Adam,MacDonald,Open,66,8,4800,6.9
Alessandro,Colantonio,Open,70,7,4274,5.2
Alessandro,Colonnier,Open,72,5,3606,4.8
Alex,McCardle,Open,73,9,,
Alexandre,Tremblay-Larochelle,Open,69,6,4922,6.1
Alistair,Campbell,Open,69,8,3049,3.8
Allan,Godding,Open,75,6,3650,4.4
Andre,Scott,Open,72,5,3232,4.7
Benjamin,King,Open,70,4,1311,1.4
Brian,Kells,Open,72,3,2520,2.5
Brian,Perry,Open,72,8,6370,7.7
Cameron,Kennedy,Open,72,8,4773,6.2
Charles,Knowles,Open,72,5,3062,4.1
Chris,Sullivan,Open,72,8,4042,5.1
Chris,Tran,Open,69,6,3950,5.5
Christo,Kutrovsky,Open,66,6,2690,4.6
Christopher,Castonguay,Open,71,7,3430,4.3
Chun,Chang,Open,69,5,2725,3.9
Colin,Scarffe,Open,71,8,4967,5.6
Cory,Boucher,Open,73,8,4298,4.8
Craig,Anderson,Open,67,8,3340,4.2
Dan,Thomson,Open,70,7,3416,5.4
Dante,LaFontaine,Open,72,7,4448,5.5
David,Townsend,Open,71,5,3629,5.0
Erik,L'Abbe,Open,75,8,3839,5.4
Ethan,Brady,Open,71,4,1863,2.2
Étienne,Pépin,Open,72,6,4114,5.5
Geoff,Solomon,Open,66,3,3117,3.8
Geofford,Seaborn,Open,75,8,3369,5.5
Giulian,De La Merced,Open,67,5,1993,3.9
Graham,Brown,Open,70,5,1850,2.2
Greg,Ellis,Open,71,9,,
Hadrian,Mertins-Kirkwood,Open,73,8,4820,7.2
Hugh,Podmore,Open,71,8,5052,5.7
Jamie,Wildgen,Open,72,7,3806,5.1
Jay,Lymer,Open,66,6,2310,3.2
Jay Thor,Turner,Open,70,6,2664,4.0
Jim,Robinson,Open,76,6,3602,5.1
John,Haig,Open,71,7,4059,5.1
John,Siwiec,Open,69,5,1965,2.4
Jon,Rowe,Open,73,6,3957,6.0
Jonathan,Champagne,Open,67,5,2450,4.1
Kelsey,Charie,Open,67,7,4188,4.9
Kevin,Hughes,Open,70,7,3593,5.2
Lance,Blackstock,Open,72,10,5525,6.8
Luca,Lafontaine,Open,68,8,5026,5.8
Luke,Krolak,Open,72,8,3053,3.4
Marcus,Bordage,Open,72,8,3619,5.0
Mark,Donahue,Open,72,8,3273,3.4
Martin,Cloake,Open,79,9,4728,6.3
Mathieu,Landry,Open,65,9,3832,4.8
Matt,Hogel,Open,70,10,,
Matthew,Schijns,Open,72,5,2283,3.2
Mehmet,Karman,Open,72,7,3430,4.8
Michael,O'Hare,Open,72,7,4427,6.1
Micheal,Davidson,Open,60,7,4311,4.9
Mike,Lee,Open,67,10,5027,5.9
Morgan,Howard,Open,73,6,3327,4.2
Nicholas,Aghajanian,Open,68,7,3456,4.8
Nick,Amlin,Open,72,1,3360,3.9
Nick,Theriault,Open,72,7,2978,4.0
Patrick,Kenzie,Open,66,6,3418,5.0
Patrick,Mapp,Open,73,10,4654,7.5
Paul,Klippenstein,Open,71,6,,
Ryan,Mussell,Open,73,5,2294,2.8
Sebastien,Belanger,Open,72,6,2612,3.5
Shubho Bo,Biswas,Open,66,7,3528,3.4
Simon,Walker,Open,73,8,5725,6.9
Sina,Dee,Open,66,7,4616,6.8
Stefan,Schulde,Open,66,7,,
Stephen,Close,Open,71,7,3545,4.7
Steve,Bisang,Open,69,7,2686,4.0
Thomas,Ferguson,Open,74,7,3943,4.8
Thomas,Sattolo,Open,73,6,3550,4.7
Tom,Newman,Open,74,8,5288,7.3
Travis,Davidson,Open,72,8,5447,6.8
Wing-Leung,Chan,Open,67,6,2964,5.1
Yingdi,Wu,Open,66,3,2691,4.6
Zach,St.Amour,Open,72,6,3022,3.8
"""
