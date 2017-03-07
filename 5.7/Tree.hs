module Tree where

import Data.Monoid
import Control.Monad.State

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
		deriving (Show)

{-
GHCi> numberTree (Leaf ())
Leaf 1
GHCi> numberTree (Fork (Leaf ()) () (Leaf ()))
Fork (Leaf 1) 2 (Leaf 3)
-}

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (treeStep tree) 1

treeStep :: Tree () -> State Integer (Tree Integer)
treeStep (Leaf _) = do
	a <- get
	put $ a + 1
	return $ Leaf a
treeStep (Fork l1 _ r1) = do
	l2 <- treeStep l1
	a <- get
	put (a + 1)
	r2 <- treeStep r1
	return $ Fork l2 a r2


	

