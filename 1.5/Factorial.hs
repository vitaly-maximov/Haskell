module Fact where
	doubleFact :: Integer -> Integer
	doubleFact n = if n < 1 then 1 else n * doubleFact (n - 2)