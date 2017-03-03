module Tasks where

import Data.Monoid

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor False
    mappend (Xor a) (Xor b) = Xor $ a /= b


newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' $ Just mempty
    mappend (Maybe' Nothing) b = Maybe' Nothing
    mappend a (Maybe' Nothing) = Maybe' Nothing
    mappend (Maybe' a) (Maybe' b) = Maybe' $ mappend a b



import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
	empty = []
	lookup key list = case L.find (\ (x, _) -> x == key) list of
		Nothing -> Nothing
		Just (_, value) -> Just value	
	insert key value list  = (key, value) : delete key list
	delete key = filter $ uncurry $ const . (/= key)

