module T2 where

import Data.Monoid
import Control.Monad.Writer
import Control.Monad.State

{-
GHCi> runState (writerToState $ tell "world") "hello,"
((),"hello,world")
GHCi> runState (writerToState $ tell "world") mempty
((),"world")
-}

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = do
	let (a, w) = runWriter m
	s <- get
	put $ s `mappend` w
	return a
