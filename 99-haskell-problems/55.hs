data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty


tree1 :: Tree Char
tree1 = Branch 'a' (Branch 'b' (leaf 'd')
                               (leaf 'e'))
                   (Branch 'c' Empty
                               (Branch 'f' (leaf 'g')
                                           Empty))

tree2 :: Tree Char
tree2 = leaf 'a'

tree3 :: Tree a
tree3 = Empty

tree4 :: Tree Int
tree4 = Branch 1 (Branch 2 Empty (leaf 4))
                 (leaf 2)
