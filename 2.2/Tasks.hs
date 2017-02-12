module Tasks where

doItYourself = f . g . h
f = (logBase 2)
g = (^3)
h = (max 42)

help1 :: a -> (a,b) -> a -> (b,a,a)
help2 :: a -> (a,b) -> a -> (b,a,a)
help3 :: a -> (a,b) -> a -> (b,a,a)
help4 :: a -> (a,b) -> a -> (b,a,a)
help5 :: a -> (a,b) -> a -> (b,a,a)
help6 :: a -> (a,b) -> a -> (b,a,a)
help7 :: a -> (a,b) -> a -> (b,a,a)

help1 a (b, c) d  = (c, a, b)
help2 a (b, c) d  = (c, a, d)
help3 a (b, c) d  = (c, b, a)
help4 a (b, c) d  = (c, b, d)
help5 a (b, c) d  = (c, d, a)
help6 a (b, c) d  = (c, d, b)
help7 a (b, c) d  = (c, a, a)

test pair = fst pair

-- uncurry (flip (,))