pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do
	True <- return $ x > 0
	a <- [1..x]
	b <- [a..x]
	c <- [b..x]
	True <- return $ a ^ 2 + b ^ 2 == c ^ 2
	return (a,b,c)