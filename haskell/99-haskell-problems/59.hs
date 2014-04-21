data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

hbalTree :: Int -> [Tree Char]
hbalTree 0 = [Empty]
hbalTree 1 = [mkLeaf]
hbalTree h = [mkNode l r |
              (hl,hr) <- [(h-1,h-1), (h-2,h-1), (h-1,h-2)],
              l <- hbalTree hl,
              r <- hbalTree hr]

mkLeaf :: Tree Char
mkLeaf = Branch 'x' Empty Empty

mkNode :: Tree Char -> Tree Char -> Tree Char
mkNode = Branch 'x'
