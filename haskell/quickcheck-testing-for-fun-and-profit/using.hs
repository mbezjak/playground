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

good Null = True
good (Fork _ l r) = weight l <= weight r

deleteMin (Fork x l r) = merge l r

merge l Null = l
merge Null r = r
merge l r | minElem l <= minElem r = join l r
          | otherwise              = join r l

join (Fork x l r) h = Fork x r (merge l h)

data Op = Insert Integer | DeleteMin
        deriving Show

make :: [Op] -> Tree Integer
make ops = foldl op Null ops
  where op h (Insert n) = insert n h
        op Null DeleteMin = Null
        op h DeleteMin = deleteMin h

prop_invariant ops = invariant (make ops)
prop_balanced ops = balanced (make ops)
prop_AllGood ops = good (make ops)

instance Arbitrary Op where
  arbitrary =
    frequency [(2, do n <- arbitrary; return (Insert n)),
               (1, return DeleteMin)]
