module Tasks where

{-
take 10 $ fibStream
[0,1,1,2,3,5,8,13,21,34]
-}

fibStream :: [Integer]
fibStream = 0 : 1 : (zipWith (+) fibStream $ drop 1 fibStream)

{-
succ $ Odd (-100000000000003)
Odd (-100000000000001)
-}

data Odd = Odd Integer 
  deriving (Eq, Show)

instance Enum Odd where
	succ (Odd x) = Odd (x + 2)
	pred (Odd x) = Odd (x - 2)

	toEnum x = Odd (toEnum x)
	fromEnum (Odd x) = fromEnum x

	enumFrom (Odd x) = map Odd [x, x + 2 ..]
	enumFromThen (Odd x) (Odd y) = map Odd [x, y ..]
	enumFromTo (Odd x) (Odd z) = map Odd [x, x + 2 .. z]
	enumFromThenTo (Odd x) (Odd y) (Odd z) = map Odd [x, y .. z]

{-
change 7
[[2,2,3],[2,3,2],[3,2,2],[7]]
-}

coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change n = [coin : set | coin <- coins, coin <= n, set <- change $ n - coin]

{-
change 0 = [[]]
change n = concat [map (coin :) $ change $ n - coin | coin <- coins, coin <= n]
-}

