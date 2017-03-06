data Log a = Log [String] a
	deriving (Show)

{-
GHCi> let add1Log = toLogger (+1) "added one"
GHCi> add1Log 3
Log ["added one"] 4

GHCi> let mult2Log = toLogger (* 2) "multiplied by 2"
GHCi> mult2Log 3
Log ["multiplied by 2"] 6
-}

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f str = (\ a -> Log [str] $ f a)

{-
GHCi> execLoggers 3 add1Log mult2Log
Log ["added one","multiplied by 2"] 8
-}

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers a f1 f2 = case f1 a of
	(Log logList1 b) -> case f2 b of
		(Log logList2 c) -> Log (logList1 ++ logList2) c


returnLog :: a -> Log a
returnLog = Log []