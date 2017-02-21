module Tasks where

{-
concatList [[1,2],[],[3]]
[1,2,3]
-}

concatList :: [[a]] -> [a]
concatList = foldr (++) []

{-
lengthList [7,6,5]
3
-}

lengthList :: [a] -> Int
lengthList = foldr (\ _ -> (1 +)) 0

{-
sumOdd [2,5,30,37]
42
-}

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0