module Tpoulsen.Lib where

even : Int -> Bool
even x = mod x 2 == 0

odd : Int -> Bool
odd x = mod x 2 /= 0

---LIST---
elem : a -> [a] -> Bool
elem x xs = any (\y -> y==x) xs

---MAYBE---
listToMaybe : [a] -> Maybe a
listToMaybe x = if | x == []   -> Nothing
                   | otherwise -> Just (head x)
