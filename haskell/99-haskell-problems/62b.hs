data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

tree :: Tree Int
tree = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                (Branch 3 Empty Empty)


atLevel :: Tree a -> Int -> [a]
atLevel Empty          _ = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch x l r) n = atLevel l (n-1) ++ atLevel r (n-1)
