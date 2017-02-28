module Person where

import Data.Char
import Data.List

data Error = ParsingError | IncompleteDataError | IncorrectDataError String
	deriving (Show)

data Person = Person { firstName :: String, lastName :: String, age :: Int }
	deriving (Show)

parsePerson :: String -> Either Error Person
parsePerson str = case lines str of
	[firstNameLine, lastNameLine, ageLine] -> 
		let
			maybeFirstName = stripPrefix "firstName = " firstNameLine
			maybeLastName = stripPrefix "lastName = " lastNameLine
			maybeAge = stripPrefix "age = " ageLine
		in case (maybeFirstName, maybeLastName, maybeAge) of
			(Just firstName, Just lastName, Just age) -> case all isDigit age of
				True -> Right $ Person firstName lastName $ read age
				False -> Left $ IncorrectDataError age
			_ -> Left IncompleteDataError
	_ -> Left ParsingError