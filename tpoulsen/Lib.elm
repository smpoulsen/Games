module Tpoulsen.Lib where

---LIST---
elem : a -> [a] -> Bool
elem x xs = any (\y -> y==x) xs

---MAYBE---
listToMaybe : [a] -> Maybe a
listToMaybe x = if | x == []   -> Nothing
                   | otherwise -> Just (head x)
