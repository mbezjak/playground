data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)


layout :: Tree a -> Tree (a, (Int, Int))
layout tree = grid 1 (maxDepth tree) tree
  where
    grid _ _ Empty          = Empty
    grid h d (Branch x l r) =
      let p  = 2 ^ (d - h) - 1
          h' = h + 1
          l' = grid h' d l
          r' = grid h' d r
      in Branch (x, (p, h)) l' r'

maxDepth :: Tree a -> Int
maxDepth Empty = 0
maxDepth (Branch _ l r) = 1 + max (maxDepth l) (maxDepth r)

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )
