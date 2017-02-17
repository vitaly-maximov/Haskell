module Tasks where

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b xs = a : b : xs

nTimes:: a -> Int -> [a]
nTimes a 0 = []
nTimes a n = a : (nTimes a (n - 1))

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs) | odd x = x : rest
				  | True = rest
			where rest = oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 as bs cs = ((h0 as) + (h0 bs) + (h0 cs)) : (sum3 (t0 as) (t0 bs) (t0 cs))
	where		
		h0 [] = 0
		h0 (x : xs) = x

		t0 [] = []
		t0 (x : xs) = xs

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems xs = (take count xs) : groupElems (drop count xs)
	where
		count = countSame xs
		
		countSame (x : y : xys) | x == y = 1 + countSame (y : xys)
		countSame (x : _) = 1