newtype Pair b a = Pair { getPair :: (a,b) } deriving (Show)

instance Functor (Pair c) where
--  fmap f p = let (a,b) = getPair p in Pair (f a, b)
  fmap f (Pair (x,y)) = Pair (f x, y)
