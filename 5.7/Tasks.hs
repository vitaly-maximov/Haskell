import Data.Monoid
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

evalWriter :: Writer w a -> a
evalWriter = fst . runWriter

{-
GHCi> total shopping1 
19708
-}

type Shopping = Writer (Sum Integer) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

purchase :: String -> Integer -> Shopping
purchase item cost = tell $ Sum cost

total :: Shopping -> Integer
total = getSum . execWriter

readerToState :: Reader r a -> State r a
readerToState m = state (\ r -> (runReader m r, r))