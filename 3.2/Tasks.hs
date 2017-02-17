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