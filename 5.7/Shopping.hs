import Data.Monoid
import Control.Monad.Writer

{-
GHCi> total shopping1 
19708
GHCi> items shopping1
["Jeans","Water","Lettuce"]
-}

type Shopping = Writer (Sum Integer, [String]) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

purchase :: String -> Integer -> Shopping
purchase item cost = tell (Sum cost, [item])

total :: Shopping -> Integer
total = getSum . fst . execWriter

items :: Shopping -> [String]
items = snd. execWriter