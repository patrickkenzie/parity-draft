module Format exposing(formatSalary)

import String exposing(..)

formatSalary : Int -> String
formatSalary value =
   String.cons '$' (String.join "," (chunksOfRight 3 <| toString value))

chunksOfRight : Int -> String -> List String
chunksOfRight k s =
  let len = length s
      k2 = 2 * k
      chunksOfR sub =
        if length sub > k2
        then right k sub :: chunksOfR (dropRight k sub)
        else right k sub :: [dropRight k sub]
  in  if len > k2 then
          List.reverse (chunksOfR s)
      else if len > k then
          dropRight k s :: [right k s]
      else
          [s]
