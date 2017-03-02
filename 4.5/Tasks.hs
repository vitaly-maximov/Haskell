module Tasks where

data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : (fromList xs)

toList :: [a] -> List a
toList [] = Nil
toList (x : xs) = Cons x (toList xs)


data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add a Zero = a
add a (Suc b) = add (Suc a) b

mul :: Nat -> Nat -> Nat
mul _ Zero = Zero
mul Zero _ = Zero
mul a (Suc Zero) = a
mul a (Suc b) = accumulate a b where
	accumulate sum Zero = sum
	accumulate sum (Suc count) = accumulate (add sum a) count

fac :: Nat -> Nat
fac Zero = Suc Zero
fac (Suc Zero) = Suc Zero
fac a1@(Suc a0) = mul a1 $ fac a0


data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node left right) = 1 + max (height left) (height right)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node left right) = 1 + (size left) + (size right)


avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf a) = (1, a)
    go (Node a b) = case (go a, go b) of
    	((ac, as), (bc, bs)) -> (ac + bc, as + bs)


infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand e@(Val _ :*: Val _) = e
expand e@(Val _ :+: Val _) = e
expand ((e1 :+: e2) :*: e) = expand (e1 :*: e) :+: expand (e2 :*: e)
expand (e :*: (e1 :+: e2)) = expand (e :*: e1) :+: expand (e :*: e2)
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand (expand e1 :*: expand e2)
expand e = e