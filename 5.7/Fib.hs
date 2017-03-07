module Fib where

import Data.Monoid
import Control.Monad.State

{-
GHCi> execState fibStep (0,1)
(1,1)
GHCi> execState fibStep (1,1)
(1,2)
GHCi> execState fibStep (1,2)
(2,3)
-}

fibStep :: State (Integer, Integer) ()
fibStep = do
	(a, b) <- get
	put (b, a + b)

execStateN :: Int -> State s a -> s -> s
execStateN n m s = foldr (\ _ -> execState m) s [1..n]

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)