module Operator where
	infixl 6 |-|
	a |-| b = abs (a - b)