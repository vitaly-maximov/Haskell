data Board = ABC

nextPositions :: Board -> [Board]
nextPositions = undefined

--Тип Board и функция nextPositions заданы, реализовывать их не нужно

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred = do
	False <- return $ n < 0
	if (n == 0) then do
		True <- return $ pred b
		return b
	else do
		nextBoard <- nextPositions b
		nextPositionsN nextBoard (n - 1) pred