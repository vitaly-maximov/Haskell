module Tasks where

import Data.Char(isDigit)

data Coord a = Coord a a
	deriving (Show)

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x2 - x1) + abs (y2 - y1)

getCenter :: Double -> Coord Int -> Coord Double
getCenter width (Coord x y) = Coord (transform x) (transform y) where
	transform = (width *) . (0.5 +) . fromIntegral

getCell :: Double -> Coord Double -> Coord Int
getCell width (Coord x y) = Coord (transform x) (transform y) where
	transform = floor . (/ width)


findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x : xs) | isDigit x = Just x
                   | otherwise = findDigit xs
{- findDigit = find isDigit -}

findDigitOrX :: [Char] -> Char
findDigitOrX xs = case findDigit xs of
	Nothing -> 'X'
	Just x -> x


maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x


data Error = ParsingError | IncompleteDataError | IncorrectDataError String
	deriving (Show)

data Person = Person { firstName :: String, lastName :: String, age :: Int }
	deriving (Show)

parsePerson :: String -> Either Error Person
parsePerson str = Right $ Person (get "firstName") (get "lastName") (get "age") where
	records = lines str

	get field = case find (\ (name : _) -> name == field) records of
		Nothing -> Nothing
		Just [_, _, value] -> Just value