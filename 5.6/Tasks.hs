data Reader r a = Reader { runReader :: (r -> a) }

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ (\ r -> runReader m $ f r)

{-
GHCi> runReader usersWithBadPasswords [("user", "123456"), ("x", "hi"), ("root", "123456")]
["user","root"]
-}

type User = String
type Password = String
type UsersTable = [(User, Password)]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = Reader $ map (\ (name, _) -> name) . filter (\ (name, password) -> password == "123456")

