module Tasks where

{-
take 10 $ fibStream
[0,1,1,2,3,5,8,13,21,34]
-}

fibStream :: [Integer]
fibStream = 0 : 1 : (zipWith (+) fibStream $ drop 1 fibStream)

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
