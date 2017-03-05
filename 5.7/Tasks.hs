evalWriter :: Writer w a -> a
evalWriter = fst . runWriter