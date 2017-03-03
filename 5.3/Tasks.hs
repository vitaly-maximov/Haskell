data SomeType a = SomeType a deriving (Show)

instance Monad SomeType where
	return = SomeType
	(SomeType x) >>= f = f x

instance Functor SomeType where
    fmap f x = x >>= (return . f)