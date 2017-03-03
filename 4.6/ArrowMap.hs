import Prelude hiding (lookup)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap (\ _ -> Nothing)
    lookup key (ArrowMap arrow) = arrow key
    insert key value map = ArrowMap insert' where
    	insert' x | x == key = Just value
    			  | otherwise = lookup x map
    delete key map = ArrowMap delete' where
    	delete' x | x == key = Nothing
    			  | otherwise = lookup x map
    fromList = foldr (uncurry insert) empty