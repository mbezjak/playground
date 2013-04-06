data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

construct :: [Int] -> Tree Int
construct = foldl add Empty
  where
    add Empty            x = leaf x
    add t@(Branch y l r) x =
      case compare x y of
        EQ -> t
        LT -> Branch y (add l x) r
        GT -> Branch y l (add r x)


symmetric :: Tree a -> Bool
symmetric Empty          = True
symmetric (Branch _ l r) = mirror l r

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 r2 && mirror l2 r1
mirror _ _         = False

leaf :: a -> Tree a
leaf x = Branch x Empty Empty
