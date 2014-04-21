data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)


layout :: Tree a -> Tree (a, (Int, Int))
layout = grid 1 1
  where
    grid _ _ Empty          = Empty
    grid p h (Branch x l r) =
      let h' = h + 1
          l' = grid p h' l
          p' = nextPosition p l'
          r' = grid (p'+1) h' r
      in Branch (x, (p', h)) l' r'

nextPosition :: Int -> Tree (a, (Int, Int)) -> Int
nextPosition current Empty                        = current
nextPosition _       (Branch (_, (p, _)) _ Empty) = p + 1
nextPosition current (Branch _           _ r)     = nextPosition current r


tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )
