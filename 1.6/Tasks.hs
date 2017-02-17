module Tasks where

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x == 0 = (0, 1)
			  | True = help 0 0 ux
    where
        ux = abs x
        help s c n = if (n == 0) then (s, c) else help (s + (mod n 10)) (c + 1) (div n 10)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = dx * (((f a) + (f b)) / 2 + (sum 1 h 0))
	where 
		h = 1000
		dx = (b - a) / h
		sum i n acc | i == n = acc
				  	| True = sum (i + 1) n (acc + (f (a + i * (b - a) / h)))
