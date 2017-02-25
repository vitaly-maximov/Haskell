module Tasks where

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle a b) = a * b 

data Result = Fail | Success
data SomeData = A | B | C

doSomeWork :: SomeData -> (Result,Int)
doSomeWork A = (Success, 0)
doSomeWork B = (Fail, 1)

data Result' = Success' | Fail' Int

instance Show Result' where
    show Success' = "Success"
    show (Fail' n) = "Fail: " ++ show n

doSomeWork' :: SomeData -> Result'
doSomeWork' x = case doSomeWork x of
	(Success, _) -> Success'
	(Fail, n) -> Fail' n


square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) | a == b = True
isSquare _ = False
