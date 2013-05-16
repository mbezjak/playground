data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

hbalTree :: Int -> [Tree Char]
hbalTree 0 = []
hbalTree 1 = [mkLeaf]
hbalTree 2 = [mkNode mkLeaf mkLeaf, mkNode mkLeaf Empty, mkNode Empty mkLeaf]
hbalTree n =
  concat [combination x y | x <- hbalTree (next-rem), y <- hbalTree next]
  where next = n - 1
        rem  = next `mod` 2
        combination x y =
          if rem == 0 then [mkNode x y] else [mkNode x y, mkNode y x]

mkLeaf :: Tree Char
mkLeaf = Branch 'x' Empty Empty

mkNode :: Tree Char -> Tree Char -> Tree Char
mkNode = Branch 'x'
