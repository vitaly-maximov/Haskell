module Tasks where

import Data.List

lastElem :: [a] -> a
lastElem = foldl1 $ flip const

{-
revRange ('a','z')
"zyxwvutsrqponmlkjihgfedcba"
-}

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g (a, b) | a > b = Nothing
  				 | otherwise = Just (b, (a, pred b))