module Main where

import System.IO
import System.Directory
import Data.List

main = main'

{-
main' :: IO ()
main' = do
	putStr "Substring: "
	hFlush stdout
	substring <- getLine
	if 
		null substring 
	then 
		putStrLn "Canceled"
	else do
		files <- getDirectoryContents "."
		mapM remove $ filter (isInfixOf substring) files 
		return ()
		where
			remove file = do
				putStrLn $ "Removing file: " ++ file
				removeFile file
-}

main' :: IO ()
main' = do
	putStr "Substring: "
	hFlush stdout
	substring <- getLine
	if 
		null substring 
	then 
		putStrLn "Canceled"
	else do
		files <- getDirectoryContents "."
		return { 
		do
			file <- files
			True <- return $ isInfixOf substring file
			do
				putStrLn $ "Removing file: " ++ file
				removeFile file
		}

{-		
		do
			file <- files
			True <- return $ isInfixOf substring file
			do
				putStrLn $ "Removing file: " ++ file
				removeFile file
-}