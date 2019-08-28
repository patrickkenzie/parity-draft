module Format exposing (chunksOfRight, formatHeight, formatRating)

import String exposing (..)


formatHeight : Int -> String
formatHeight value =
    let
        feet =
            value // 12

        inches =
            remainderBy 12 value

        feetDisplay =
            String.fromInt feet ++ "'"

        inchesDisplay =
            String.fromInt inches ++ "\""
    in
    "height: " ++ feetDisplay ++ " " ++ inchesDisplay


formatRating : Int -> String
formatRating rating =
    "rating: " ++ String.fromInt rating


chunksOfRight : Int -> String -> List String
chunksOfRight k s =
    let
        len =
            length s

        k2 =
            2 * k

        chunksOfR sub =
            if length sub > k2 then
                right k sub :: chunksOfR (dropRight k sub)

            else
                right k sub :: [ dropRight k sub ]
    in
    if len > k2 then
        List.reverse (chunksOfR s)

    else if len > k then
        dropRight k s :: [ right k s ]

    else
        [ s ]
