module LogEntry where

import Data.Time.Clock
import Data.Time.Format
import System.Locale

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString entry = 
	(entry & timestamp & timeToString) ++ ": " ++ 
	(entry & logLevel & logLevelToString) ++ ": " ++
	(entry & message)