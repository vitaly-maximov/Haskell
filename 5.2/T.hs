data Log a = Log [String] a
	deriving (Show)

{-
GHCi> Log ["nothing done yet"] 0 `bindLog` add1Log
Log ["nothing done yet","added one"] 1

GHCi> Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
Log ["nothing done yet","added one","multiplied by 2"] 8
-}

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log log1 a) f = case f a of
	(Log log2 b) -> Log (log1 ++ log2) b

{-
GHCi> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
Log ["added one","multiplied by 2","multiplied by 100"] 800
-}

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f str = (\ a -> Log [str] $ f a)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers a f1 f2 = case f1 a of
	(Log logList1 b) -> case f2 b of
		(Log logList2 c) -> Log (logList1 ++ logList2) c

returnLog :: a -> Log a
returnLog = Log []

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . return