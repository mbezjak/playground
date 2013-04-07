data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree


symmetric :: Tree a -> Bool
symmetric Empty          = True
symmetric (Branch _ l r) = mirror l r

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 r2 && mirror l2 r1
mirror _ _         = False

cbalTree :: Int -> [Tree Char]
cbalTree 0 = []
cbalTree 1 = [mkLeaf]
cbalTree 2 = [mkNode mkLeaf Empty, mkNode Empty mkLeaf]
cbalTree n | rem == 0  = [mkNode l r | l <- cbalTree half, r <- cbalTree half]
           | otherwise =
  [mkNode l r | l <- cbalTree (half+rem), r <- cbalTree half] ++
  [mkNode l r | l <- cbalTree half, r <- cbalTree (half+rem)]
  where (half, rem) = (n-1) `quotRem` 2

mkLeaf :: Tree Char
mkLeaf = leaf 'x'

mkNode :: Tree Char -> Tree Char -> Tree Char
mkNode = Branch 'x'

leaf :: a -> Tree a
leaf x = Branch x Empty Empty
