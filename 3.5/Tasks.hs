module Tasks where

{-
meanList [1,2,3,4]
2.5
-}

meanList :: [Double] -> Double
meanList = uncurry (/) . foldr (\x (sum, length) -> (x + sum, 1 + length)) (0, 0)

{-
evenOnly [1..10]
[2,4,6,8,10]
evenOnly ['a'..'z']
"bdfhjlnprtvxz"
-}

evenOnly :: [a] -> [a]
evenOnly = foldr skipOdd [] . zip [1..] where
	skipOdd (i, x) xs | odd i = xs
					  | otherwise = x : xs


{-
evenOnly xs = map fst $ filter (uncurry $ const even) $ zip xs [1..]
-}

{-
evenOnly xs = map fst . filter (uncurry $ const even) . fst . foldr (\ x (xs, i) -> ((x, i) : xs, i - 1)) ([], length xs) $ xs
-}

{-
evenOnly xs = fst $ foldr skipOdd ([], length xs) xs where
	skipOdd x (xs, i) | odd i = (xs, i - 1)
					  | otherwise = (x : xs, i - 1)
-}
