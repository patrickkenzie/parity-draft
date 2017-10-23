module Format exposing (..)

import String exposing (..)


formatHeight : Int -> String
formatHeight value =
    let
        feet =
            value // 12

        inches =
            rem value 12

        feetDisplay =
            (toString feet) ++ "'"

        inchesDisplay =
            (toString inches) ++ "\""
    in
        "height: " ++ feetDisplay ++ " " ++ inchesDisplay


formatRating : Int -> String
formatRating rating =
    "rating: " ++ (toString rating)


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
