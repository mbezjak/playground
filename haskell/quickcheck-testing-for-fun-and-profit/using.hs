import Test.QuickCheck

prop_revApp xs ys =
  reverse (xs++ys) == reverse ys ++ reverse xs


data Tree a = Null | Fork a (Tree a) (Tree a)
                     deriving (Eq, Show)

empty = Null

invariant Null = True
invariant (Fork x l r) = smaller x l && smaller x r
smaller x Null = True
smaller x (Fork y l r) = x <= y && invariant (Fork y l r)

minElem (Fork x _ _) = x

insert x Null = Fork x Null Null
insert x (Fork y l r) = Fork (min x y) r (insert (max x y) l)

balanced Null = True
balanced (Fork _ l r) = (d==0 || d==1) && balanced l && balanced r
  where d = weight r - weight l

weight Null = 0
weight (Fork _ l r) = 1 + weight l + weight r

make :: [Integer] -> Tree Integer
make ns = foldl (\h n -> insert n h) empty ns

prop_invariant ns = invariant (make ns)
prop_balanced ns = balanced (make ns)
