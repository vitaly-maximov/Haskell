sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 as bs cs = ((h0 as) + (h0 bs) + (h0 cs)) : (sum3 (t0 as) (t0 bs) (t0 cs))
	where		
		h0 [] = 0
		h0 (x : xs) = x

		t0 [] = []
		t0 (x : xs) = xs

groupElems :: Eq a => [a] -> [[a]]
groupElems xs = help [] [] xs
	where 
		help result [] [] = reverse result
		help result group (x : y : xys) | x == y = help result (x : group) (y : xys)
		help result group (x : xs = help (group : result) [] xs
		help result [] (x : xs) = help ([x] : result) [] xs
		help result group xs = help (group : result) [] xs
		
