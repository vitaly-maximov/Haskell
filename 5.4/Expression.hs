import Data.Char

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
	deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken str | all isDigit str = Just $ Number $ read str
asToken _ = Nothing

tokenize :: String -> Maybe [Token]
tokenize input = foldr f (return []) (words input) where 
	f word list = do
		token <- asToken word
		tokens <- list
		return $ token : tokens