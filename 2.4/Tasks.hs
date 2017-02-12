module Tasks where

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | doesEnrageMork a && doesEnrageGork a = stomp (stab a)
    			  | doesEnrageMork a = stomp a
    			  | doesEnrageGork a = stab a
    			  | True = a

a = 127.22
b = 4.1
c = 20.1
d = 2

ip = show a ++ show b ++ show c ++ show d

class (Eq a, Enum a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a | a == maxBound = minBound
          | True = succ a

  spred :: a -> a
  spred a | a == minBound = maxBound
  		  | True = pred a

avg :: Int -> Int -> Int -> Double
avg a b c = fromRational (toRational (toInteger a + toInteger b + toInteger c)) / 3.0

-- avg maxBound maxBound maxBound
-- 9.223372036854776e18

-- avg a b c = fromRational (toRational (a + b + c)) / 3.0

-- avg maxBound maxBound maxBound
-- 3.0744573456182584e18

