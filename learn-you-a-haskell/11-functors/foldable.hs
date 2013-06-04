import qualified Data.Foldable as F
import Data.Monoid

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)


{- from source of Data.Monoid:
newtype Endo a = Endo { appEndo :: a -> a }

instance Monoid (Endo a) where
  mempty = Endo id
  Endo f `mappend` Endo g = Endo (f . g)
-}

{- from source of Data.Foldable:
class Foldable t where
  ...
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f z t = appEndo (foldMap (Endo . f) t) z
  ...
-}

instance F.Foldable Tree where
  foldMap f Empty        = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           f x           `mappend`
                           F.foldMap f r

testTree = Node 5
             (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
             )
             (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
             )
