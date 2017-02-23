module Tasks where

data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"


charToInt :: Char -> Int
charToInt c | (c >= '0') && (c <= '9') = fromEnum c - fromEnum '0'


stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue


{-
cmp Error Warning
GT
cmp Info Warning
LT	
-}

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Error _  = GT
cmp _ Info = GT
cmp _ _ = LT


data Result = Fail | Success

data SomeData = A | B | C

doSomeWork :: SomeData -> (Result,Int)
doSomeWork A = (Success, 0)
doSomeWork B = (Fail, 1)
doSomeWork C = (Fail, 2)

processData :: SomeData -> String
processData x = case doSomeWork x of
	(Success, _) -> "Success"
	(Fail, n) -> "Fail: " ++ (show n) 