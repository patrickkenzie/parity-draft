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
Alisha,Zhao,Woman,64,10,4662,6.7
Alix,Ranger,Woman,67,6,1306,3.2
Alyssa,Mainwood,Woman,68,5,,
Andrea,Dietz,Woman,67,8,2202,3.2
Angela,Mueller,Woman,69,7,3007,5.5
Cassie,Berquist,Woman,66,9,2078,4
Charlotte,Mussells,Woman,66,4,,
Corinne,Dunwoody,Woman,68,9,,
Dana,Strauss,Woman,65,7,,
Deborah,Murphy,Woman,65,6,2696,4.9
Elisa,Mantil,Woman,69,5,1780,4.1
Erin,Courtney,Woman,64,6,2509,4
Genevieve,Labelle,Woman,70,8,4112,5.8
Hannah,Lewis,Woman,68,6,,
Heather,Wallace,Woman,64,6,4115,4.8
Isabelle,Potvin,Woman,70,6,2359,3.6
Jaime,Boss,Woman,66,8,2426,3.1
Josee,Guibord,Woman,64,9,4183,6.3
Julia,Riddick,Woman,70,5,2408,4.3
Justine,Dagenais,Woman,64,6,4228,6
Justine,Price,Woman,63,7,4791,6.5
Kaleigh,Newson,Woman,62,6,,
Kama,Szereszewski,Woman,63,7,887,1.3
Karen,Kavanagh,Woman,64,7,1872,3.4
Kate,Achtell,Woman,61,6,3983,6.2
Kate,Cavallaro,Woman,67,8,3012,3.4
Katelyn,Fontaine,Woman,68,5,,
Kristyn,Berquist,Woman,64,5,1047,4
Laura Chambers,Storey,Woman,68,7,2687,4.6
Linh,Pham-Vo,Woman,62,3,967,2.9
Liz,Love,Woman,66,7,,
Marie-Ange,Gravel,Woman,66,6,2688,4.4
Melissa,Jess,Woman,64,8,2610,5
Natalie,Mullin,Woman,68,6,3784,4.6
Neena,Sidhu,Woman,63,6,1857,4
Rachel,Hurdle,Woman,67,7,2672,4.1
Rachel,Lefebvre,Woman,70,5,1244,1.3
Rachel,Ng,Woman,60,5,3218,4.2
Sarah,Thompson,Woman,69,3,,
Stephanie,Jack,Woman,67,5,,
Abe,Greenspoon,Open,69,6,,
Ainsley,Bernard,Open,70,4,,
Alessandro,Colantonio,Open,70,7,4048,5.1
Alessandro,Colonnier,Open,72,5,2986,5
Alexandre,Tremblay-Larochelle,Open,69,6,,
Alistair,Campbell,Open,69,8,,
Allan,Godding,Open,75,6,3506,4.9
Andre,Scott,Open,72,5,2862,4.6
Benjamin,King,Open,70,4,,
Brian,Kells,Open,72,3,2852,3.3
Brian,Perry,Open,72,8,6413,8.1
Cameron,Kennedy,Open,72,8,,
Charles,Knowles,Open,72,5,,
Chris,Sullivan,Open,72,8,4349,5.6
Chris,Tran,Open,69,6,,
Christo,Kutrovsky,Open,66,6,2747,5.2
Christopher,Castonguay,Open,71,7,3553,4
Chun,Chang,Open,69,5,,
Colin,Scarffe,Open,71,8,,
Cory,Boucher,Open,73,8,,
Craig,Anderson,Open,67,8,5241,5.8
Dan,Thomson,Open,70,7,4060,6
Dante,LaFontaine,Open,72,7,,
David,Townsend,Open,71,5,3495,6.5
Erik,L'Abbe,Open,75,8,,
Ethan,Brady,Open,71,4,,
Etienne,Pepin,Open,72,6,,
Geoff,Solomon,Open,66,3,,
Geofford,Seaborn,Open,75,8,3161,5.9
Giulian,De La Merced,Open,67,5,3062,5.5
Graham,Brown,Open,70,5,2118,3.6
Greg,Probe,Open,70,7,2884,4.7
Hadrian,Mertins-Kirkwood,Open,73,8,6509,8
Hugh,Podmore,Open,71,8,5339,7.4
Jamie,Wildgen,Open,72,7,4390,6.3
Jay Thor,Turner,Open,70,6,2061,4.7
Jim,Robinson,Open,76,6,3300,4.9
John,Haig,Open,71,7,4599,5.5
John,Siwiec,Open,69,5,2078,3.9
Jon,Rowe,Open,73,6,4978,6.3
Jonathan,Champagne,Open,67,5,2189,3.5
Kelsey,Charie,Open,67,7,4747,6.4
Kevin,Hughes,Open,70,7,3639,5.4
Lance,Blackstock,Open,72,10,5070,7.1
Luke,Krolak,Open,72,8,,
Marcus,Bordage,Open,72,8,4697,6.4
Mark,Donahue,Open,72,8,3213,4.1
Martin,Cloake,Open,79,9,3969,5.3
Martin,Shiu,Open,68,8,,
Mathieu,Landry,Open,65,9,,
Matthew,Schijns,Open,72,5,2812,5.5
Mehmet,Karman,Open,72,7,2839,4.4
Michael,Colantonio,Open,70,5,3000,5.2
Michael,O'Hare,Open,72,7,4427,6.1
Micheal,Davidson,Open,60,7,4534,6.6
Mike,Lee,Open,67,10,5095,6.4
Morgan,Howard,Open,73,6,3608,5.5
Nicholas,Aghajanian,Open,68,7,3613,5.9
Nicholas,Belanger,Open,70,6,,
Nick,Amlin,Open,72,8,3739,5
Nick,Klimowicz,Open,74,6,3152,4.4
Nick,Theriault,Open,72,7,3872,6.2
Pascal,Michaud,Open,69,8,4253,5.9
Patrick,Kenzie,Open,66,6,3248,5.3
Patrick,Mapp,Open,73,10,5349,7.4
Paul,Morrison,Open,56,5,,
Ryan,Mussell,Open,73,5,,
Sebastien,Belanger,Open,72,6,3569,5.7
Shubho Bo,Biswas,Open,66,7,3539,4.2
Simon,Walker,Open,73,8,,
Sina,Dee,Open,66,7,,
Stephen,Close,Open,71,7,4081,6.2
Steve,Bisang,Open,69,7,3527,5.4
Thomas,Ferguson,Open,74,7,4136,5.7
Thomas,Sattolo,Open,73,6,2103,3.4
Tom,Newman,Open,74,8,5076,7.2
Travis,Davidson,Open,72,8,5759,7.6
Wing-Leung,Chan,Open,67,6,2405,3.8
Yingdi,Wu,Open,66,3,2303,3.1
Zach,St.Amour,Open,72,6,,
"""
