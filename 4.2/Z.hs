module Z (z, normalize, negate', add, subtract', mul) where

import Data.List

data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

instance Show Bit where
	show Zero = "0"
	show One = "1"

instance Eq Bit where
	(==) a b = show a == show b 

instance Ord Bit where
	compare One Zero = GT
	compare Zero One = LT
	compare _ _ = EQ

instance Show Sign where
	show Minus = "-"
	show Plus = "+"

instance Eq Sign where
	(==) a b = show a == show b

instance Show Z where
	show (Z sign []) = "Z " ++ show sign ++ "0"
	show (Z sign xs) = "Z " ++ show sign ++ (foldr (flip (++) . show) "" xs)

instance Eq Z where
	(==) z1 z2 = case (normalize z1, normalize z2) of
		(Z _ [], Z _ []) -> True
		(nz1, nz2) -> show nz1 == show nz2

instance Ord Z where
	compare (Z Plus _) (Z Minus _) = GT
	compare (Z Minus _) (Z Plus _) = LT
	compare (Z Minus bits1) (Z Minus bits2) = compare (Z Plus bits2) (Z Plus bits1)
	compare z1 z2 = 
		let
			(Z _ bits1) = normalize z1
			(Z _ bits2) = normalize z2
		in case compare (length bits1) (length bits2) of
			EQ -> compareBits (reverse bits1) (reverse bits2) where
				compareBits [] [] = EQ
				compareBits (x : xs) (y : ys) = case compare x y of
					EQ -> compareBits xs ys
					order -> order
			order -> order

z :: String -> Z
z ('Z' : ' ' : sign : bits@(_ : _)) = 
	normalize $ Z (readSign sign) (foldr (flip (++) . (: []) . readBit) [] bits) where
		readBit :: Char -> Bit
		readBit '0' = Zero
		readBit '1' = One
		readBit char = error $ "cannot parse a bit: " ++ [char]

		readSign :: Char -> Sign
		readSign '-' = Minus
		readSign '+' = Plus
		readSign char = error $ "cannot parse a sign: " ++ [char]		
z string = error $ "cannot parse a Z: " ++ string

normalize :: Z -> Z
normalize (Z sign bits) = Z sign $ reverse $ dropWhile (== Zero) $ reverse bits

negate' :: Z -> Z
negate' (Z Plus xs) = Z Minus xs
negate' (Z Minus xs) = Z Plus xs

zip' :: [Bit] -> [Bit] -> [(Bit, Bit)]
zip' [] [] = []
zip' (x : xs) [] = (x, Zero) : zip' xs []
zip' [] (y : ys) = (Zero, y) : zip' [] ys
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

add :: Z -> Z -> Z
add z1@(Z Plus _) (Z Minus bits2) = subtract' z1 (Z Plus bits2)
add z1@(Z Minus bits1) z2@(Z Plus _) = subtract' z2 (Z Plus bits1)
add z1 z2 = 
	let
		(Z sign bits1) = normalize z1
		(Z _ bits2) = normalize z2
	in normalize $ Z sign $ addBits (zip' bits1 bits2) Zero where
		addBit :: Bit -> Bit -> Bit -> (Bit, Bit)
		addBit Zero Zero Zero = (Zero, Zero)
		addBit Zero Zero One = (One, Zero)
		addBit Zero One Zero = (One, Zero)
		addBit One Zero Zero = (One, Zero)
		addBit One One One = (One, One)
		addBit _ _ _ = (Zero, One)

		addBits :: [(Bit, Bit)] -> Bit -> [Bit]
		addBits [] overflow = [overflow]
		addBits ((x, y) : xys) overflow = (fst sum) : addBits xys (snd sum) where
			sum = addBit x y overflow

subtract' :: Z -> Z -> Z
subtract' z1@(Z sign1 _) z2@(Z sign2 _) | sign1 /= sign2 = add z1 $ negate' z2
subtract' z1@(Z Minus _) z2@(Z Minus _) = subtract' (negate' z2) (negate' z1)
subtract' z1 z2 | z1 < z2 = negate' $ subtract' z2 z1
subtract' z1 z2 = 
	let
		(Z _ bits1) = normalize z1
		(Z _ bits2) = normalize z2
	in normalize $ Z Plus $ subtractBits (zip' bits1 bits2) Zero where	
		subtractBit :: Bit -> Bit -> Bit -> (Bit, Bit)
		subtractBit Zero One One = (Zero, One)
		subtractBit One Zero Zero = (One, Zero)
		subtractBit Zero Zero Zero = (Zero, Zero)
		subtractBit One Zero One = (Zero, Zero)
		subtractBit One One Zero = (Zero, Zero)
		subtractBit _ _ _ = (One, One)

		subtractBits :: [(Bit, Bit)] -> Bit -> [Bit]
		subtractBits [] _ = []
		subtractBits ((x, y) : xys) overflow = (fst diff) : subtractBits xys (snd diff) where
			diff = subtractBit x y overflow

mul :: Z -> Z -> Z  
mul (Z sign1 bits1) (Z sign2 bits2) | sign1 /= sign2 = negate' $ mul (Z Plus bits1) (Z Plus bits2)
mul z1 z2 =
	let
		(Z _ bits1) = normalize z1
		(Z _ bits2) = normalize z2
	in snd $ foldl' mulBits (bits1, Z Plus []) bits2 where
		mulBits :: ([Bit], Z) -> Bit -> ([Bit], Z)
		mulBits (bits, sum) Zero = (Zero : bits, sum)
		mulBits (bits, sum) One = (Zero : bits, add (Z Plus bits) sum)
