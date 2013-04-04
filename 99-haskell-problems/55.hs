data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving Eq

instance Show a => Show (Tree a) where
  show Empty = ""
  show (Branch x Empty Empty) = show x
  show (Branch x l Empty)     = show x ++ " " ++ show l ++ " -"
  show (Branch x Empty r)     = show x ++ " - " ++ show r
  show (Branch x l r)         = show x ++ " (" ++ show l ++ ") (" ++ show r ++ ")"

leaf :: a -> Tree a
leaf x = Branch x Empty Empty


mkLeaf :: Tree Char
mkLeaf = leaf 'x'

mkNode :: Tree Char -> Tree Char -> Tree Char
mkNode = Branch 'x'

cbalTree :: Int -> [Tree Char]
cbalTree 0 = []
cbalTree 1 = [mkLeaf]
cbalTree 2 = [mkNode mkLeaf Empty, mkNode Empty mkLeaf]
cbalTree n | rem == 0  = [mkNode l r | l <- cbalTree half, r <- cbalTree half]
           | otherwise =
  [mkNode l r | l <- cbalTree (half+rem), r <- cbalTree half] ++
  [mkNode l r | l <- cbalTree half, r <- cbalTree (half+rem)]
  where (half, rem) = (n-1) `quotRem` 2
