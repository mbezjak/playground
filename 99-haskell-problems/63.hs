data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)


completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = atAddress 1
  where atAddress a
          | a <= n    = Branch 'x' (atAddress (2*a)) (atAddress (2*a+1))
          | otherwise = Empty
