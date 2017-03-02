data Point3D a = Point3D a a a deriving Show

{-
fmap (+ 1) (Point3D 5 6 7)
Point3D 6 7 8
-}

instance Functor Point3D where
    fmap f (Point3D a b c) = Point3D (f a) (f b) (f c)


data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
	deriving (Show)

{-
fmap (+ 1) $ Point (Point3D 0 0 0)
Point (Point3D 1 1 1)
fmap (+ 1) $ LineSegment (Point3D 0 0 0) (Point3D 1 1 1)
LineSegment (Point3D 1 1 1) (Point3D 2 2 2)
-}

instance Functor GeomPrimitive where
    fmap f (Point a) = Point $ fmap f a
    fmap f (LineSegment a b) = LineSegment (fmap f a) (fmap f b)


data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

{-
words <$> Leaf Nothing
Leaf Nothing
words <$> Leaf (Just "a b")
Leaf (Just ["a","b"])
-}

instance Functor Tree where
    fmap f (Leaf a) = Leaf $ fmap f a    
    fmap f (Branch left a right) = Branch (fmap f left) (fmap f a) (fmap f right)