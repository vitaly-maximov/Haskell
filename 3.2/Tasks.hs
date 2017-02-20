module Tasks where

import Data.Char

readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 xs = filter (\x -> (p1 x) || (p2 x)) xs

{- 
filterDisj _ _ [] = []
filterDisj p1 p2 (x : xs) 
	| (p1 x) || (p2 x) = x : rest
	| True = rest
		where
			rest = filterDisj p1 p2 xs
-}

{- 
qsort [1,3,2,5]
[1,2,3,5]
-}

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort xs' @ (x : xs) = (qsort l) ++ e ++ (qsort g)
	where 
		l = filter (\a -> a < x) xs'
		e = filter (\a -> a == x) xs'
		g = filter (\a -> a > x) xs'

-- squares'n'cubes :: Num a => [a] -> [a]
-- squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])


{-
perms [1,2,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-}

{-
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = iterate [] x xs  
	where
		iterate [] x [] = [[x]]
		iterate start x [] = (map (\p -> (x : p)) (perms start))
		iterate start x end = (map (\p -> (x : p)) (perms (start ++ end))) ++ (iterate (start ++ [x]) (head end) (tail end))
-}

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = concatMap (\ (x, rs) -> map (x :) $ perms rs) $ iterate [] xs
	where 
		iterate _ [] = []
		iterate hs (x : ts) = (x, hs ++ ts) : iterate (x : hs) ts

delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

{-
max3 [7,2,9] [3,6,8] [1,8,10]
[7,8,10]
-}

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 [] [] [] = []
max3 (a : as) (b : bs) (c :cs) = (max a (max b c)) : max3 as bs cs
