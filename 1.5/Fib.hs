module Fib where
	fibonacci :: Integer -> Integer
	fibonacci n | n == 0 = 0
				| (n == 1) || (n == -1) = 1
				| n > 0	= fibonacci (n - 2) + fibonacci (n - 1)
				| n < 0	= fibonacci (n + 2) - fibonacci (n + 1)

	fib2 :: Integer -> Integer
	fib2 n | n == 0 = 0
		   | (n == 1) || (n == -1) = 1
		   | (n > 0) = help1 1 1 1 n
		   | (n < 0) = help2 0 1 (-1) n

	help1 a b k n = if (k < n) then help1 b (a + b) (k + 1) n else a
	help2 a b k n = if (k > n) then help2 b (a - b) (k - 1) n else b