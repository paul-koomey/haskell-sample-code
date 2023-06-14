-- exploring modules

import Data.List
import Data.Char


numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub




-- Caesar cipher
encode :: Int -> String -> String
encode shift msg = 
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted


decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg


-- find  a value based on its key from a dictionary, this is the 
-- same as the lookup function from Data.List
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key [] = Nothing
findKey ((k,v):xs) = if key = k
                        then Just v
                        else findKey key xs
